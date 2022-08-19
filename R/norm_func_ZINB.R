#' Function to run TaxNorm algorithm
#' @name TaxNorm_Normalization
#' @param data (Required) Input data; should be either a phyloseq object or a count matrix
#' @param depth sequencing depth if pre-calculated. It should be a vector with the same length and order as the column of the count data
#' @param group condition variables if samples are from multiple groups; should be correpsond to the column of the count data. default is NULL, where no grouping is considered
#' @param filter.sample.num,filter.taxa.count taxa with "filter.taxa.count" in more than "filter.sample.num" samples will be removed before testing. default is keep taxa appear in at least 10 samples within each group
#' @param random calculate randomized normal quantile residual
#' @param ncores whether multiple cores is used for parallel computing; default is max(1, detectCores() - 1)
#'
#' @return a list containing the normalized count values
#'
#' @examples
#' TaxNorm_Normalization()
#'
#' @export

TaxNorm_Normalization <- function(data,
                     depth = NULL, group = NULL,
                     filter.sample.num = 10,
                     filter.taxa.count = 0,
                     random = TRUE,
                     ncores = NULL) {

  if (!(methods::is(data, "phyloseq")) & !(is.matrix(data))) {
    stop("Input data must be either a phyloseq object or a count matrix.")}

  if (is.matrix(data)) {
    if (is.null(rownames(data) | is.null(colnames(data)))) {
      stop("Must supply taxa/sample names.")}
  }

  if(methods::is(data, "phyloseq")) {
    count <- abundances(data)
  }else {
    count <- data
  }

  if(!is.null(group)) {
    if(!is.factor(group)) group=as.factor(group)
    if(length(group) != ncol(count)) stop("The number of conditions is not the same as the number of samples. ")
  }

  ## filter rare taxa; default is keep taxa appear in at least 10 samples within each group
  if(!is.null(group)) {
    taxaIn <- lapply(levels(as.factor(group)), function(i) {
      which(apply(count[, which(group == i)], 1, function(x) sum(x > filter.taxa.count) > filter.sample.num))})
    taxaIn <- Reduce(intersect, taxaIn)
  }else {
    taxaIn <- apply(count, 1, function(x) sum(x > filter.taxa.count) > filter.sample.num)
  }

  count_toUse <- count[taxaIn,]
  message(paste0("Removing ", (nrow(count) - nrow(count_toUse)), " rare taxa... \n" ))


  ## run normalization algorithm
  # fit zero-inflated negative binomial regression for each taxa, get the estimated parameters
  message(paste0("Fitting models... \n" ))

  if(is.null(depth)){
    depth_toUse <- colSums(count_toUse)
  }else depth_toUse <- depth


  if (is.null(ncores)) {ncores <- max(1, parallel::detectCores() - 1)}
  if(ncores == 1){
    model_pars_list <- apply(count_toUse, 1, fit_zinb, depth = depth_toUse, covar = group)
  }else{
    message(paste0("Setting up parallel computation using ", ncores, " cores... \n" ))
    if (.Platform$OS.type == "windows" | parallelly::supportsMulticore() == FALSE) {
      future::plan(future::multisession, workers=ncores)}
    else {
      future::plan(future::multicore, workers=ncores)}
    model_pars_list <- future.apply::future_apply(count_toUse, 1, fit_zinb, depth = depth_toUse, covar = group,
                                                  future.seed = FALSE)
  }

  converge <- sapply(model_pars_list, function(x) x$converge)

  # estimated parameters
  coefficients <- t(sapply(model_pars_list, function(x) x$coefficients))

  theta <- t(sapply(model_pars_list, function(x) x$theta))
  mu <- t(sapply(model_pars_list, function(x) x$mu))
  pi <- t(sapply(model_pars_list, function(x) x$pi))
  pi[is.na(pi)] <- 0

  # obtain randomized quantile residuals as the normalized count
  normdata_list <- list()
  for (i in 1:nrow(count_toUse)) {
    normdata_list[[i]] <- quantile_match(count = count_toUse[i,],
                                         mu = mu[i,], theta = theta[i,], pi = pi[i,],
                                         random = random)
  }
  names(normdata_list) <- rownames(count_toUse)

  ecdf <- do.call("rbind", lapply(normdata_list, function(x) x$pvalue))
  normdata <- do.call("rbind", lapply(normdata_list, function(x) x$count_norm))
  colnames(normdata) <- colnames(count_toUse)

  return(list(rawdata = count_toUse,
              normdata = normdata, ecdf = ecdf,
              model_pars = list(coefficients = coefficients,
                                mu = mu, theta = theta, pi = pi),
              converge = converge))
}

## function to fit ZINB regression
fit_zinb <- function(count, depth, covar){ ## include covariates

  # scale depth
  depth <- log(depth)

  if(is.null(covar)) {
    fit <- suppressWarnings(zinb.reg(formula = count ~ depth | depth | depth,
                                     control = zinb.control(trace=FALSE)))
  }else {
    # add group cov - difference between biological groups
    fit <- suppressWarnings(zinb.reg(formula = count ~ depth + covar | depth | depth,
                                     control = zinb.control(trace=FALSE)))
  }
  converge = fit$converged

  # beta0, beta1, beta2, alpha0, alpha1, kappa0, kappa1
  coeff <- fit$coefficients

  # estimated parameters
  mu <- fit$estimates$mu
  mu[is.infinite(mu)] <- ifelse(all(is.infinite(mu)), max(count), pmax(mu[!is.infinite(mu)], max(count)))

  # make sure theta is not too small or infinite
  theta <- fit$estimates$theta
  theta[is.infinite(theta)] <- ifelse(all(is.infinite(theta)), 1e7, pmax(theta[!is.infinite(theta)], 1e7))

  pi <- fit$estimates$pi


  # depth-adjusted parameters
  if(is.null(covar)) {
    mu_adj <- mu
  }else {
    mu_adj <- exp(coeff[1] + coeff[2] * depth)
    mu_adj[is.infinite(mu_adj)] <- ifelse(all(is.infinite(mu_adj)), max(count), pmax(mu_adj[!is.infinite(mu_adj)], max(count)))
  }
  pi_adj <- pi
  theta_adj <- theta

  return(list(coefficients = coeff,
              mu = mu_adj, theta = theta_adj, pi = pi_adj,
              converge = converge))

}

## function to calculate equivalent corrected quantiles
quantile_match <- function(count, mu, theta, pi, random = FALSE){

  n <- length(count)

  dznbinom <- function(x, theta, mu, pi){
    return((1-pi) * dnbinom(x, size = theta, mu = mu) + pi * (x == 0))
  }

  pznbinom <- function(x, theta, mu, pi){
    return((1-pi) * pnbinom(x, size = theta, mu = mu) + pi * (x >= 0))
  }

  if(!random) {
    var <- (1-pi) * (mu + (mu^2/theta)) + mu^2 * (pi^2 + pi)
    count_norm <- (count - (1-pi)*mu)/sqrt(var)
    colnames(count_norm) <- colnames(count)
  }else {
    pvalue <- pznbinom((count - 1), theta, mu, pi) + dznbinom(count, theta, mu, pi) * runif(n)
    pvalue <- pmin(pmax(pvalue,1e-10), (1 - 1e-10))

    # will cause NaN; need to check
    count_norm <- qnorm(pvalue)
    count_norm[count_norm == -Inf] <- min(count_norm[count_norm != -Inf])
    count_norm[count_norm == Inf] <- max(count_norm[count_norm != Inf])

    colnames(count_norm) <- colnames(count)
  }

  return(list(pvalue=pvalue, count_norm=count_norm))
}






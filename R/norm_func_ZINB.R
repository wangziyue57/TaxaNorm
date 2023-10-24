#' Function to run TaxaNorm algorithm
#' @name TaxaNorm_Normalization
#' @param data (Required) Input data; should be either a phyloseq object or a count matrix
#' @param depth sequencing depth if pre-calculated. It should be a vector with the same length and order as the column of the count data
#' @param group condition variables if samples are from multiple groups; should be correpsond to the column of the count data. default is NULL, where no grouping is considered
#' @param filter.taxa.count "filter.taxa.count" samples will be removed before testing. default is keep taxa appear in at least 10 samples within each group
#' @param filter.cell.num taxa with "filter.cell.num" in more than the value provided will be filtered
#' @param meta.data meta data for Taxa
#' @param random calculate randomized normal quantile residual
#' @param ncores whether multiple cores is used for parallel computing; default is max(1, detectCores() - 1)
#'
#' @return a TaxaNorm Object containing the normalized count values and accessory information
#' @import phyloseq microbiome matrixStats
#' @importFrom stats dnbinom .getXlevels model.matrix model.response model.weights nlm optim pchisq pnbinom qnorm runif terms update
#' @examples
#' \donttest{
#' data("TaxaNorm_Example_Input", package = "TaxaNorm")
#' Normalized_Data <- TaxaNorm_Normalization(data= TaxaNorm_Example_Input,
#'                                          depth = NULL,
#'                                          group = sample_data(TaxaNorm_Example_Input)$body_site,
#'                                          meta.data = NULL,
#'                                          filter.cell.num = 10,
#'                                          filter.taxa.count = 0,
#'                                          random = FALSE,
#'                                          ncores = 1)}


#' @export

TaxaNorm_Normalization <- function(data, depth = NULL, group = NULL, meta.data = NULL,
                                  filter.cell.num = 10,
                                  filter.taxa.count = 0,
                                  random = FALSE,
                                  ncores = NULL) {

  if (!(methods::is(data, "phyloseq")) & !(is.matrix(data))) {
    stop("Input data must be either a phyloseq object or a count matrix.")}

  if (is.matrix(data)) {
    if (is.null(rownames(data) | is.null(colnames(data)))) {
      stop("Must supply taxa/sample names.")}

    if (is.null(meta.data)) {
      stop("Must provide a meta data file.")}

    if (sum(colnames(data) != rownames(meta.data)) != 0) {
      stop("The sample names for the count matrix and the meta data do not match.")}

    # create phyloseq object
    data <- phyloseq(otu_table(data, taxa_are_rows = TRUE), sample_data(meta.data))
  }

  if(methods::is(data, "phyloseq")) {count <- abundances(data)} else {count <- data}

  if(!is.null(group)) {
    if(!is.factor(group)) group=as.factor(group)
    if(length(group) != ncol(count)) stop("The number of conditions is not the same as the number of samples. ")
  }

  ## filter rare taxa; default is keep taxa appear in at least 10 samples within each group
  if(!is.null(group)) {
    taxaIn <- lapply(levels(as.factor(group)), function(i) {
      which(apply(count[, which(group == i)], 1, function(x) sum(x > filter.taxa.count) > filter.cell.num))})
    taxaIn <- Reduce(intersect, taxaIn)
  }else {
    taxaIn <- apply(count, 1, function(x) sum(x > filter.taxa.count) > filter.cell.num)
  }
  count_toUse <- count[taxaIn,]
  message(paste0("Removing ", (nrow(count) - nrow(count_toUse)), " rare taxa... \n" ))

  if(is.null(depth)){
    depth_toUse <- colSums(count_toUse)
  }else depth_toUse <- depth

  ## run normalization algorithm
  # fit zero-inflated negative binomial regression for each taxa, get the estimated parameters
  message(paste0("Fitting models... \n" ))

  if (is.null(ncores)) {ncores <- max(1, parallel::detectCores() - 1)}
  if (ncores > 1) {
    message(paste0("Setting up parallel computation using ", ncores, " cores... \n" ))
    if (.Platform$OS.type == "windows" | parallelly::supportsMulticore() == FALSE) {
      future::plan(future::multisession, workers=ncores)} else {
        future::plan(future::multicore, workers=ncores)}

    model_pars_list <- future.apply::future_apply(count_toUse, 1, fit_zinb, depth = depth_toUse, covar = group,
                                                  future.seed = FALSE)
  }else {
    model_pars_list <- list()
    for (i in 1:nrow(count_toUse)) {
      model_pars_list[[i]] <- suppressWarnings(fit_zinb(count = count_toUse[i,], depth = depth_toUse, covar = group))
    }
  }

  converge <- sapply(model_pars_list, function(x) x$converge)
  llk <- sapply(model_pars_list, function(x) x$llk)
  df <- sapply(model_pars_list, function(x) x$df)

  # design matrix
  # X <- model_pars_list[[1]]$design$X
  # Z <- model_pars_list[[1]]$design$Z
  # M <- model_pars_list[[1]]$design$M

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


  counts_df <- as.data.frame(count_toUse)
  normdata_df <- as.data.frame(normdata)
  ecdf_df <- as.data.frame(ecdf)

  model_parameters <- TaxaNorm_Model_Parameters(coefficients = coefficients,
                                               mu = mu, theta = theta, pi = pi)

  myinput <- data

  myresults <- TaxaNorm_Results(input_data = myinput,
                               rawdata = counts_df,
                               normdata = normdata_df,
                               ecdf = ecdf_df,
                               model_pars = model_parameters,
                               converge = converge,
                               llk = llk,
                               final_df = df)

  return(myresults)

  # return(list(rawdata = counts_df,
  #             normdata = normdata_df, ecdf = ecdf_df,
  #             model_pars = list(coefficients = coefficients,
  #                               mu = mu, theta = theta, pi = pi),
  #             test_model_pars <- model_parameters,
  #             converge = converge))

}

## function to fit ZINB regression
## function to fit ZINB regression
fit_zinb <- function(count, depth, covar){ ## include covariates

  # calculate observed zeros
  zero <- sum(count==0)

  # run our model
  if(is.null(covar)) {
    if(zero>=10){
      suppressWarnings(fit <- try(zinb.reg(formula = count ~ log(depth) | 1 | log(depth),
                                           x = TRUE, control = zinb.control(trace=FALSE)),
                                  silent = TRUE))
    }else{
      suppressWarnings(fit <- try(nb.reg(formula = count ~ log(depth) | log(depth),
                                         x = TRUE, control = zinb.control(trace=FALSE)),
                                  silent = TRUE))
    }
  }else {
    # only consider mean count difference between biological groups
    if(zero>=10){
      suppressWarnings(fit <- try(zinb.reg(formula = count ~ log(depth) + covar | 1 | log(depth),
                                           x = TRUE, control = zinb.control(trace=FALSE)),
                                  silent = TRUE))
    }else{
      suppressWarnings(fit <- try(nb.reg(formula = count ~ log(depth) + covar | log(depth),
                                         x = TRUE, control = zinb.control(trace=FALSE)),
                                  silent = TRUE))
    }
  }

  #print(fit)

  # model character
  converge = fit$converged
  llk <- fit$loglik
  df <- fit$df.residual

  # design matrix
  # X <- fit$x$count
  # Z <- fit$x$zero
  # M <- fit$x$dispersion

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
  if(is.null(covar)){
    mu_adj <- mu
  }else{
    mu_adj <- exp(coeff[1] + coeff[2] * log1p(depth))
    mu_adj[is.infinite(mu_adj)] <- ifelse(all(is.infinite(mu_adj)), max(count), pmax(mu_adj[!is.infinite(mu_adj)], max(count)))
  }
  pi_adj <- pi
  theta_adj <- theta

  return(list(coefficients = coeff, #design = list(X=X, Z=Z, M=M),
              mu = mu_adj, theta = theta_adj, pi = pi_adj,
              converge = converge, llk = llk, df = df))
}


### function to calculate equivalent corrected quantiles
quantile_match <- function(count, mu, theta, pi, random = FALSE){

  n <- length(count)

  dznbinom <- function(x, theta, mu, pi){
    return((1-pi) * dnbinom(x, size = theta, mu = mu) + pi * (x == 0))
  }

  pznbinom <- function(x, theta, mu, pi){
    return((1-pi) * pnbinom(x, size = theta, mu = mu) + pi * (x >= 0))
  }

  if(!random) {
    pvalue <- pznbinom((count - 1), theta, mu, pi)
    count_norm <- qnorm(pvalue)
    #count_norm <- countreg::qzinbinom(pvalue, mu = ceiling(mu), theta = theta, pi = pi)

    # for outlier count, if p==1, will return Inf values -> use original count instead
    count_norm[which(abs(pvalue-1) < 1e-4)] <- count[which(abs(pvalue-1) < 1e-4)]
    colnames(count_norm) <- colnames(count)

  }else {
    set.seed(n)
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

########################
# utilities...
########################
model_offset_2 <- function (x, terms = NULL, offset = TRUE)  {
  if (is.null(terms))
    terms <- attr(x, "terms")
  offsets <- attr(terms, "offset")
  if (length(offsets) > 0) {
    ans <- if (offset)
      x$"(offset)"
    else NULL
    if (is.null(ans))
      ans <- 0
    for (i in offsets) ans <- ans + x[[deparse(attr(terms,
                                                    "variables")[[i + 1]])]]
    ans
  } else {
    ans <- if (offset)
      x$"(offset)"
    else NULL
  }
  if (!is.null(ans) && !is.numeric(ans))
    stop("'offset' must be numeric")
  ans
}

zinb.control <- function (maxIter = 200, tol = 1e-5, trace = TRUE, start = NULL, EM = TRUE) {
  rval <- list(maxIter = maxIter, trace = trace, tol = tol, start = start, EM = EM)
  rval
}

nb.control <- function (maxIter = 200, tol = 1e-5, trace = TRUE, start = NULL) {
  rval <- list(maxIter = maxIter, trace = trace, tol = tol, start = start)
  rval
}


########################
# likelihood functions...
########################
# complete likelihood of ZINB for EM algorithm
loglikc <- function (paras, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, w, offsetz, offsetx, offsetm) {

  # Make sure all the coefficients are in matrix format
  beta.pi <- paras[pi.ind]
  beta.mu <- paras[mu.ind]
  beta.theta <- paras[theta.ind]

  eta.pi <- as.vector(X.pi %*% beta.pi + offsetz)
  eta.mu <- as.vector(X.mu %*% beta.mu + offsetx)
  eta.theta <- as.vector(X.theta %*% beta.theta + offsetm)

  pi <- exp(eta.pi) / (1+exp(eta.pi)) # if exp(eta.pi) is large, pi -> 1, then llk_temp -> inf
  mu <- exp(eta.mu)
  theta <- exp(eta.theta)

  llk_temp <- w * log(pi) + (1 - w) * log(1 - pi) + (1 - w) * suppressWarnings(dnbinom(y, mu = mu, size = theta, log = TRUE))
  llk_temp[which(pi==1)] <- 0
  llk_temp[which(pi==0)] <- suppressWarnings(dnbinom(y, mu = mu, size = theta, log = TRUE))

  llk = - sum(llk_temp, na.rm = TRUE)

  return(llk)
}

# original llk for ZINB
loglik0 <- function (paras, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, offsetz, offsetx, offsetm) {

  # Make sure all the coefficients are in matrix format
  beta.pi <- paras[pi.ind]
  beta.mu <- paras[mu.ind]
  beta.theta <- paras[theta.ind]

  eta.pi <- as.vector(X.pi %*% beta.pi + offsetz)
  eta.mu <- as.vector(X.mu %*% beta.mu + offsetx)
  eta.theta <- as.vector(X.theta %*% beta.theta + offsetm)

  pi <- exp(eta.pi) / (1+exp(eta.pi)) # if exp(eta.pi) is large, pi -> 1, then llk_temp -> 0
  mu <- exp(eta.mu)
  theta <- exp(eta.theta)

  I <- as.numeric(y == 0)

  llk_temp <- log(pi * I + (1 - pi) * suppressWarnings(dnbinom(y, mu = mu, size = theta)))
  llk_temp[is.infinite(llk_temp)] = 0

  llk = -sum(llk_temp, na.rm = TRUE)

  return(llk)
}


# original llk for NB
loglik0.nb <- function (paras, mu.ind, theta.ind, X.mu, X.theta, y, offsetx, offsetm) {

  beta.mu <- paras[mu.ind]
  beta.theta <- paras[theta.ind]

  eta.mu <- as.vector(X.mu %*% beta.mu + offsetx)
  eta.theta <- as.vector(X.theta %*% beta.theta + offsetm)

  mu <- exp(eta.mu)
  theta <- exp(eta.theta)

  llk_temp <- suppressWarnings(dnbinom(y, mu = mu, size = theta, log = TRUE))
  llk = -sum(llk_temp, na.rm = TRUE)

  return(llk)
}


########################
# EM algorithm...
########################
Estep <- function (paras, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, offsetz, offsetx, offsetm) {

  # Make sure all the coefficients are in matrix format
  beta.pi <- paras[pi.ind]
  beta.mu <- paras[mu.ind]
  beta.theta <- paras[theta.ind]

  eta.pi <- as.vector(X.pi %*% beta.pi + offsetz)
  eta.mu <- as.vector(X.mu %*% beta.mu + offsetx)
  eta.theta <- as.vector(X.theta %*% beta.theta + offsetm)

  pi <- exp(eta.pi) / (1+exp(eta.pi))
  mu <- exp(eta.mu)
  theta <- exp(eta.theta)

  ind <- y == 0
  y <- y[ind]
  pi <- pi[ind]
  mu <- mu[ind]
  theta <- theta[ind]

  w <- numeric(length(y))
  w[ind] <- pi  / (pi  + (1 - pi) * suppressWarnings(dnbinom(y, mu = mu, size = theta)))

  return(w)
}

Mstep <- function (obj.func, beta0, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, w, offsetz, offsetx, offsetm,
                   optim_method = "BFGS", ...) {

  opt.obj <- optim(beta0, obj.func, gr = NULL,
                   pi.ind=pi.ind, mu.ind=mu.ind, theta.ind=theta.ind,
                   X.pi=X.pi, X.mu=X.mu, X.theta=X.theta,
                   y=y, w=w,
                   offsetz=offsetz, offsetx=offsetx, offsetm=offsetm,
                   method = optim_method,
                   control = list(maxit = 200, trace = 0), ...)
  return(opt.obj)
}


########################
# Zeroinflated negative binomial regression with covariate-dependent dispersion
# Formula can be specified for three components (count, zero, and dispersion) respectively
# e.g. Y ~ X  | X | X .
# no boundary for coef's
#########################
zinb.reg <- function (formula, data, subset, na.action, weights, offset,
                      optim_method = "L-BFGS-B",
                      control = zinb.control(),
                      model = TRUE, y = TRUE, x = TRUE, ...) {

  cl <- match.call()

  if (missing(data)) {
    data <- environment(formula)
  }

  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]],
                                            as.name("|"))) {
    ff <- formula
    formula[[3]][1] <- call("+")
    formula[[3]][[2]][1] <- call("+")
    mf$formula <- formula
    ffc <- . ~ .
    ffz <- ~.
    ffm <- ~.
    ffc[[2]] <- ff[[2]]
    ffc[[3]] <- ff[[3]][[2]][[2]]
    ffz[[3]] <- ff[[3]][[2]][[3]]
    ffz[[2]] <- NULL
    ffm[[3]] <- ff[[3]][[3]]
    ffm[[2]] <- NULL
  } else {
    ffz <- ffc <- ffm <- ff <- formula
    ffz[[2]] <- ffm[[2]] <- NULL
  }

  if (inherits(try(terms(ffz), silent = TRUE), "try-error")) {
    ffz <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])),
                                     deparse(ffz))))
  }
  if (inherits(try(terms(ffm), silent = TRUE), "try-error")) {
    ffm <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])),
                                     deparse(ffm))))
  }

  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")

  mtX <- terms(ffc, data = data)
  X <- model.matrix(mtX, mf)

  mtZ <- terms(ffz, data = data)
  mtZ <- terms(update(mtZ, ~.), data = data)
  Z <- model.matrix(mtZ, mf)

  mtM <- terms(ffm, data = data)
  mtM <- terms(update(mtM, ~.), data = data)
  M <- model.matrix(mtM, mf)

  Y <- model.response(mf, "numeric")
  if (length(Y) < 1) {
    stop("empty model")
  }
  if (any(Y < 0)) {
    stop("invalid dependent variable, negative counts")
  }

  if (control$trace) {
    message("dependent variable:\n")
    tab <- table(Y)
    names(dimnames(tab)) <- NULL
    print(tab)
  }

  n <- length(Y)
  kx <- NCOL(X)
  kz <- NCOL(Z)
  km <- NCOL(M)
  Y0 <- Y <= 0
  Y1 <- Y > 0

  weights <- model.weights(mf)

  if (is.null(weights)) {
    weights <- 1
  }
  if (length(weights) == 1) {
    weights <- rep.int(weights, n)
  }
  weights <- as.vector(weights)
  names(weights) <- rownames(mf)

  offsetx <- model_offset_2(mf, terms = mtX, offset = TRUE)
  if (is.null(offsetx)) {
    offsetx <- 0
  }
  if (length(offsetx) == 1) {
    offsetx <- rep.int(offsetx, n)
  }
  offsetx <- as.vector(offsetx)


  offsetz <- model_offset_2(mf, terms = mtZ, offset = FALSE)
  if (is.null(offsetz)) {
    offsetz <- 0
  }
  if (length(offsetz) == 1) {
    offsetz <- rep.int(offsetz, n)
  }
  offsetz <- as.vector(offsetz)

  offsetm <- model_offset_2(mf, terms = mtM, offset = FALSE)
  if (is.null(offsetm)) {
    offsetm <- 0
  }
  if (length(offsetm) == 1) {
    offsetm <- rep.int(offsetm, n)
  }
  offsetm <- as.vector(offsetm)


  # init start values
  start <- control$start
  if (!is.null(start)) {
    valid <- TRUE
    if (!("count" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, count model coefficients not specified")
      start$count <- rep.int(0, kx)
    }
    if (!("zero" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, zero-inflation model coefficients not specified")
      start$zero <- rep.int(0, kz)
    }
    if (!("theta" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, dispersion model coefficients not specified")
      start$zero <- rep.int(0, km)
    }
    if (length(start$count) != kx) {
      valid <- FALSE
      warning("invalid starting values, wrong number of count model coefficients")
    }
    if (length(start$zero) != kz) {
      valid <- FALSE
      warning("invalid starting values, wrong number of zero-inflation model coefficients")
    }
    if (length(start$theta) != km) {
      valid <- FALSE
      warning("invalid starting values, wrong number of dispersion coefficients")
    }

    if (!valid) {
      start <- NULL
    }
  }

  if (is.null(start)) {
    if (control$trace) {
      message("generating starting values...\n")
    }

    # fit built-in function
    mod.init <- pscl::zeroinfl(formula = Y ~ 1 | 1, offset = offsetx,
                               dist="negbin", EM = FALSE)
    start$count <- c(mod.init$coefficients$count, rep(0, kx - 1))
    start$theta <- c(log(mod.init$theta), rep(0, km - 1))

    p0 <- sum(Y==0)/n
    start$zero <- c((log(p0) - log(1-p0)), rep(0, kz - 1))
  }
  paras0 <- c(start$zero, start$count, start$theta)


  # EM iterations...
  pi.ind <- 1:kz
  mu.ind <- (kz + 1) : (kz + kx)
  theta.ind <- (kz + kx + 1) : (kz + kx + km)

  if (control$EM) {

    if (control$trace) {message("EM estimation:\n")}

    w0 <- exp(Z %*% start$zero + offsetz) / (1+exp(Z %*% start$zero + offsetz))
    w0[Y != 0] <- 0

    Q0 <- loglikc(paras0, pi.ind, mu.ind, theta.ind, Z, X, M, Y, w = w0, offsetz, offsetx, offsetm)

    nIter <- 0
    while (TRUE) {
      if (control$trace) {
        message(Q0, '\n')
      }
      nIter <- nIter + 1

      w1 <- Estep(paras0, pi.ind, mu.ind, theta.ind,
                  X.pi=Z, X.mu=X, X.theta=M, y=Y, offsetz, offsetx, offsetm)
      M.obj <- try(Mstep(loglikc, paras0, pi.ind, mu.ind, theta.ind,
                         X.pi=Z, X.mu=X, X.theta=M, y=Y, w=w1, offsetz, offsetx, offsetm, hessian=FALSE,
                         optim_method = optim_method),
                   silent = TRUE)
      if(inherits(M.obj,"try-error")==TRUE) break

      paras1 <- M.obj$par
      Q1 <- M.obj$value

      if (Q1==0 | abs(Q1 - Q0) / abs(Q0) < control$tol | nIter >= 200) break # problem: will return Q1=0 complete llk;

      Q0 <- Q1
      paras0 <- paras1
    }

    # obtain the hessian
    M.obj <- try(Mstep(loglikc, paras0, pi.ind, mu.ind, theta.ind,
                       X.pi=Z, X.mu=X, X.theta=M, y=Y, w=w1, offsetz, offsetx, offsetm, hessian=TRUE,
                       optim_method = optim_method),
                 silent = TRUE)
    fit <- M.obj

  }else{

    # optimize llk directly
    fit <- try(optim(paras0, loglik0, gr = NULL,
                     pi.ind, mu.ind, theta.ind,
                     X.pi=Z, X.mu=X, X.theta=M, y=Y, offsetz, offsetx, offsetm,
                     method = "L-BFGS-B", hessian=TRUE,
                     control = list(maxit = 200, trace = 0)), silent = TRUE)
  }

  if (control$trace) {
    message('Finished!\n')
  }

  # summarize...
  if(inherits(fit,"try-error")==FALSE){

    # mle estimations
    fit$nIter <- nIter
    paras1 <- fit$par
    fit$loglik <- -loglik0(paras1, pi.ind, mu.ind, theta.ind, Z, X, M, Y, offsetz, offsetx, offsetm)

    if (fit$convergence > 0) {
      warning("optimization failed to converge\n")
      fit$converged <- FALSE
    } else {
      fit$converged <- TRUE
    }

    coefz <- fit$par[1:kz]
    names(coefz) <- names(start$zero) <-colnames(Z)

    coefc <- fit$par[(kz + 1):(kx + kz)]
    names(coefc) <- names(start$count) <- colnames(X)

    coefm <- fit$par[(kx + kz + 1):(kx + kz + km)]
    names(coefm) <- names(start$theta) <- colnames(M)

    mu <- exp(X %*% coefc + offsetx)[, 1]
    pi <- exp(Z %*% coefz + offsetz) / (1+exp(Z %*% coefz + offsetz))[,1]
    theta <- exp(M %*% coefm + offsetm)[, 1]

    # sd
    vc <- try(solve(as.matrix(fit$hessian)), silent = TRUE)
    if(inherits(vc,"try-error")==FALSE){
      colnames(vc) <- rownames(vc) <- c(paste("zero", colnames(Z), sep = "."),
                                        paste("count", colnames(X), sep = "."),
                                        paste("dispersion", colnames(M), sep = "."))

      renames <- c(paste("count", colnames(X), sep = "."),
                   paste("zero", colnames(Z), sep = "."),
                   paste("dispersion", colnames(M), sep = "."))
      vc <- vc[renames, renames]
    }else vc <- matrix(NA, (kx+kz+km), (kx+kz+km))


    # others
    Yhat <- (1 - pi) * mu
    res <- sqrt(weights) * (Y - Yhat)
    nobs <- sum(weights > 0)

    rval <- list(coefficients = c(coefc, coefz, coefm),
                 estimates = list(mu = mu, pi = pi, theta = theta),
                 residuals = res, fitted.values = Yhat, vcov=vc,
                 optfit = fit, control = control, start = start,
                 weights = if (identical(as.vector(weights),rep.int(1L, n))) NULL else weights,
                 offset = list(count = if (identical(offsetx, rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz,rep.int(0, n))) NULL else offsetz, dispersion = if (identical(offsetm, rep.int(0, n))) NULL else offsetm),
                 n = nobs, df.null = nobs - 3, df.residual = nobs - (kx + kz + km),
                 terms = list(count = mtX, zero = mtZ, dispersion = mtM, full = mt),
                 loglik = fit$loglik, converged = fit$converged, call = cl, formula = ff,
                 levels = .getXlevels(mt, mf), contrasts = list(count = attr(X, "contrasts"), zero = attr(Z, "contrasts"), dispersion = attr(M, "contrasts")))
  }

  if(inherits(fit,"try-error")==TRUE){
    rval <- list(coefficients=rep(NA, (kx + kz + km)), estimates=list(mu = NA, pi = NA, theta = NA), vcov=NA, loglik=NA,
                 optfit = fit, df.null = n - 3, df.residual = n - (kx + kz + km))
  }

  if (model) {
    rval$model <- mf
  }

  if (y) {
    rval$y <- Y
  }

  if (x) {
    rval$x <- list(count = X, zero = Z, dispersion = M)
  }
  class(rval) <- "zinb.reg"
  return(rval)
}


########################
# Negative binomial regression with covariate-dependent dispersion
# Formula can be specified for two components (count and dispersion) respectively
# e.g. Y ~ X | X.
########################
nb.reg <- function (formula, data, subset, na.action, weights, offset,
                    control = nb.control(),
                    model = TRUE, y = TRUE, x = TRUE, ...) {

  cl <- match.call()

  if (missing(data)) {
    data <- environment(formula)
  }

  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "na.action", "weights", "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE

  if (length(formula[[3]]) > 1 && identical(formula[[3]][[1]],
                                            as.name("|"))) {
    ff <- formula
    formula[[3]][1] <- call("+")
    mf$formula <- formula
    ffc <- . ~ .
    ffm <- ~.
    ffc[[2]] <- ff[[2]]
    ffc[[3]] <- ff[[3]][[2]]
    ffm[[3]] <- ff[[3]][[3]]
    ffm[[2]] <- NULL
  } else {
    ffm <- ffc <- ff <- formula
    ffm[[2]] <- NULL
  }

  if (inherits(try(terms(ffm), silent = TRUE), "try-error")) {
    ffm <- eval(parse(text = sprintf(paste("%s -", deparse(ffc[[2]])),
                                     deparse(ffm))))
  }

  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")

  mtX <- terms(ffc, data = data)
  X <- model.matrix(mtX, mf)

  mtM <- terms(ffm, data = data)
  mtM <- terms(update(mtM, ~.), data = data)
  M <- model.matrix(mtM, mf)

  Y <- model.response(mf, "numeric")

  if (length(Y) < 1) {
    stop("empty model")
  }

  Y <- as.integer(round(Y + 0.001))

  if (any(Y < 0)) {
    stop("invalid dependent variable, negative counts")
  }

  if (control$trace) {
    message("dependent variable:\n")
    tab <- table(Y)
    names(dimnames(tab)) <- NULL
    print(tab)
  }

  n <- length(Y)
  kx <- NCOL(X)
  km <- NCOL(M)

  weights <- model.weights(mf)

  if (is.null(weights)) {
    weights <- 1
  }
  if (length(weights) == 1) {
    weights <- rep.int(weights, n)
  }
  weights <- as.vector(weights)
  names(weights) <- rownames(mf)

  offsetx <- model_offset_2(mf, terms = mtX, offset = TRUE)
  if (is.null(offsetx)) {
    offsetx <- 0
  }
  if (length(offsetx) == 1) {
    offsetx <- rep.int(offsetx, n)
  }
  offsetx <- as.vector(offsetx)


  offsetm <- model_offset_2(mf, terms = mtM, offset = FALSE)
  if (is.null(offsetm)) {
    offsetm <- 0
  }
  if (length(offsetm) == 1) {
    offsetm <- rep.int(offsetm, n)
  }
  offsetm <- as.vector(offsetm)


  start <- control$start
  if (!is.null(start)) {
    valid <- TRUE
    if (!("count" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, count model coefficients not specified")
      start$count <- rep.int(0, kx)
    }

    if (!("theta" %in% names(start))) {
      valid <- FALSE
      warning("invalid starting values, dispersion model coefficients not specified")
      start$theta <- rep.int(0, km)
    }
    if (length(start$count) != kx) {
      valid <- FALSE
      warning("invalid starting values, wrong number of count model coefficients")
    }

    if (length(start$theta) != km) {
      valid <- FALSE
      warning("invalid starting values, wrong number of dispersion coefficients")
    }

    if (!valid) {
      start <- NULL
    }
  }

  if (is.null(start)) {
    if (control$trace) {
      message("generating starting values...\n")
    }
    # offset confusion, to circumvent, we use it as a covariate
    # Supply a different starting point
    m0 <- MASS::glm.nb(Y ~ 1 + offsetx)
    start$count <- c(m0$coefficients[1], rep(0, kx - 1))
    start$theta <- c(log(m0$theta), rep(0, km - 1))


  }
  if (control$trace) {
    message("begin model fitting...\n")
  }
  mu.ind <- 1 : kx
  theta.ind <- (kx + 1) : (kx + km)

  paras0 <- c(start$count, start$theta)

  nlm.obj <- nlm(loglik0.nb, p=paras0, mu.ind=mu.ind, theta.ind=theta.ind, X.mu=X, X.theta=M, y=Y,
                 offsetx=offsetx, offsetm=offsetm, hessian=TRUE, ...)

  fit <- nlm.obj
  paras1 <- nlm.obj$estimate
  fit$loglik <- - loglik0.nb(paras1, mu.ind, theta.ind, X, M, Y, offsetx, offsetm)

  if (sum(!(fit$code %in% c(1, 2)))) {
    warning("optimization failed to converge in some iterations!\n")
    fit$converged <- FALSE
  } else {
    fit$converged <- TRUE
  }
  if (control$trace) {
    if (fit$converged) {
      message("model converged...\n")
    } else {
      message("model not converged...\n")
    }

  }
  coefc <- fit$estimate[(1):(kx)]
  names(coefc) <- names(start$count) <- colnames(X)

  coefm <- fit$estimate[(kx + 1):(kx + km)]
  names(coefm) <- names(start$theta) <- colnames(M)

  vc <- try(solve(as.matrix(fit$hessian)), silent = TRUE)
  if(inherits(vc,"try-error")==FALSE){
    colnames(vc) <- rownames(vc) <-
      c(paste("count", colnames(X), sep = "."),
        paste("dispersion", colnames(M), sep = "."))
  }else vc <- matrix(NA, (kx+km), (kx+km))

  mu <- exp(X %*% coefc + offsetx)[, 1]
  theta <- exp(M %*% coefm + offsetm)[, 1]

  coefz <- NA
  pi <- rep(0,length(mu))

  Yhat <- mu
  res <- sqrt(weights) * (Y - Yhat)
  nobs <- sum(weights > 0)
  rval <- list(coefficients = c(coefc, coefz, coefm),
               estimates = list(mu = mu, pi = pi, theta = theta),
               residuals = res, fitted.values = Yhat, nlmfit = fit,
               control = control, start = start,
               weights = if (identical(as.vector(weights), rep.int(1L, n))) NULL else weights,
               offset = list(count = if (identical(offsetx,  rep.int(0, n))) NULL else offsetx,
                             dispersion = if (identical(offsetm, rep.int(0, n))) NULL else offsetm),
               n = nobs, df.null = nobs - 2, df.residual = nobs - (kx + km),
               terms = list(count = mtX, dispersion = mtM, full = mt),
               loglik = fit$loglik,
               converged = fit$converged, call = cl, formula = ff,
               levels = .getXlevels(mt, mf), contrasts = list(count = attr(X, "contrasts"), dispersion = attr(M, "contrasts")))
  if (model) {
    rval$model <- mf
  }

  if (y) {
    rval$y <- Y
  }

  if (x) {
    rval$x <- list(count = X, dispersion = M)
  }
  class(rval) <- "nb.reg"
  return(rval)
}




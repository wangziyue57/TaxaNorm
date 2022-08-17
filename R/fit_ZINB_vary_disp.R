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

zinb.control <- function (maxIter = 200, tol = 1e-5, trace = TRUE, start = NULL, ndeps = 1e-5) {
  rval <- list(maxIter = maxIter, trace = trace, tol = tol, start = start, ndeps = ndeps)
  rval
}

# Zero inflated negative binomial regression with covariate-dependent dispersion
# Formula can be specified for three components (count, zero, and dispersion) respectively
# e.g. Y ~ Group + offset(offsetx) | Group + offset(offsetz) | Group + offset(offsetm).
zinb.reg <- function (formula, data, subset, na.action, weights, offset, 
                      link = c("logit", "probit", "cloglog", "cauchit", "log"), 
                      control = zinb.control(), 
                      model = TRUE, y = TRUE, x = FALSE, ...) {
  
  # Augmented likelihood
  loglik <- function (paras, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, a, offsetz, offsetx, offsetm) {
    # Make sure all the coefficients are in matrix format
    beta.pi <- paras[pi.ind]
    beta.mu <- paras[mu.ind]
    beta.theta <- paras[theta.ind]
    
    eta.pi <- as.vector(X.pi %*% beta.pi + offsetz)
    eta.mu <- as.vector(X.mu %*% beta.mu + offsetx)
    eta.theta <- as.vector(X.theta %*% beta.theta + offsetm)
    
    pi <- exp(eta.pi) / (1+exp(eta.pi))
    pi[pi == 0] = 1e-10
    pi[pi == 1] = 1-1e-10
    mu <- exp(eta.mu)
    theta <- exp(eta.theta)
    
    log_nb = suppressWarnings(dnbinom(y, mu = mu, size = theta, log = TRUE))
    log_nb[is.infinite(log_nb)] = 0
    
    llk = - sum(a * log(pi) + (1 - a) * log(1 - pi) + (1 - a) * log_nb, na.rm = TRUE)
    if(!is.finite(llk)) llk = 0
    return(llk)  
  }
  
  # Original likelihood
  loglik0 <- function (paras, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, offsetz, offsetx, offsetm) {
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
    
    I <- as.numeric(y == 0)
    sum(log(pi * I + (1 - pi) * suppressWarnings(dnbinom(y, mu = mu, size = theta))))
  }
  
  
  Estep <- function (paras, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, offsetz, offsetx, offsetm) {
    # Make sure all the coefficients are in matrix format
    # Make sure y not all zeros or nonzeros
    beta.pi <- paras[pi.ind]
    beta.mu <- paras[mu.ind]
    beta.theta <- paras[theta.ind]
    
    eta.pi <- as.vector(X.pi %*% beta.pi + offsetz)
    eta.mu <- as.vector(X.mu %*% beta.mu + offsetx)
    eta.theta <- as.vector(X.theta %*% beta.theta + offsetm)
    
    # Use more generic link function
    pi <- exp(eta.pi) / (1+exp(eta.pi))
    mu <- exp(eta.mu)
    theta <- exp(eta.theta)
    
    a <- numeric(length(y))
    ind <- y == 0
    
    y <- y[ind]
    pi <- pi[ind]
    mu <- mu[ind]
    theta <- theta[ind]
    
    a[ind] <- pi  / (pi  + (1 - pi) * suppressWarnings(dnbinom(y, mu = mu, size = theta)))
    a
  }
  
  Mstep <- function (obj.func, beta0, pi.ind, mu.ind, theta.ind, X.pi, X.mu, X.theta, y, a, offsetz, offsetx, offsetm, ...) {
    ## need to check: make sure the bound has the same length with beta0!!!
    if(length(pi.ind) == length(mu.ind)){
      opt.obj <- optim(beta0, obj.func, gr = NULL,
                       pi.ind=pi.ind, mu.ind=mu.ind, theta.ind=theta.ind, 
                       X.pi=X.pi, X.mu=X.mu, X.theta=X.theta, y=y, a=a, 
                       offsetz=offsetz, offsetx=offsetx, offsetm=offsetm,
                       method = "L-BFGS-B",
                       lower = c(rep(-Inf, length(pi.ind)), rep(0, length(mu.ind)), rep(-Inf, length(theta.ind))),
                       upper = rep(Inf, length(beta0)), ...)
    }
    else{
      opt.obj <- optim(beta0, obj.func, gr = NULL,
                       pi.ind=pi.ind, mu.ind=mu.ind, theta.ind=theta.ind, 
                       X.pi=X.pi, X.mu=X.mu, X.theta=X.theta, y=y, a=a, 
                       offsetz=offsetz, offsetx=offsetx, offsetm=offsetm,
                       method = "L-BFGS-B",
                       lower = c(rep(-Inf, length(pi.ind)), rep(0, (length(mu.ind)-1)), -Inf, rep(-Inf, length(theta.ind))),
                       upper = c(rep(Inf, length(pi.ind)), Inf, rep(10, (length(mu.ind)-2)), Inf, rep(Inf, length(theta.ind))), 
                       control = list(trace = 0,
                                      ndeps = rep(1e-5, length(beta0)),
                                      maxit = 100,
                                      REPORT = 10))
    }
    
    
    opt.obj
  }
  
  
  linkstr <- match.arg(link)
  linkobj <- make.link(linkstr)
  linkinv <- linkobj$linkinv
  link <- linkobj$link
  
  if (control$trace) {
    cat("Zero-inflated Count Model\n", paste("count model: negative binomial model",
                                             "with log link\n"), paste("zero-inflation model: binomial with", 
                                                                       linkstr, "link\n"), paste("dispersion model: log link\n"), sep = "")
  }
  
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
    cat("dependent variable:\n")
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
      cat("generating starting values...\n")
    }
    # initial another way...
    model_zero <- glm.fit(Z, as.integer(Y0), family = binomial(link = "logit"))
    model_count <- glm.fit(X, Y, family = poisson(link = "log"))
    start$zero <- model_zero$coefficients
    start$count <- model_count$coefficients
    start$theta <- c(1, rep(0, km - 1))
    
  }
  if (control$trace) {
    cat("EM estimation:\n")
  }
  
  pi.ind <- 1:kz
  mu.ind <- (kz + 1) : (kz + kx)
  theta.ind <- (kz + kx + 1) : (kz + kx + km)
  
  beta0 <- c(start$zero, start$count, start$theta)
  a0 <- exp(start$zero[1] + offsetz) / (1+exp(start$zero[1] + offsetz))
  
  a0[Y != 0] <- 0
  Q0 <- loglik(beta0, pi.ind, mu.ind, theta.ind, Z, X, M, Y, a = a0, offsetz, offsetx, offsetm)
  
  nIter <- 0
  while (TRUE) {
    if (control$trace) {
      cat(Q0, '\n')
    }
    nIter <- nIter + 1
    a1 <- Estep(beta0, pi.ind, mu.ind, theta.ind, Z, X, M, Y, offsetz, offsetx, offsetm)
    M.obj <- Mstep(loglik, beta0, pi.ind, mu.ind, theta.ind, Z, X, M, Y, a1, offsetz, offsetx, offsetm, hessian=FALSE)
    beta1 <- M.obj$par
    Q1 <- M.obj$value
    
    if (abs(Q1 - Q0) / abs(Q0+1e-10) < control$tol | nIter >= control$maxIter) break
    
    Q0 <- Q1
    beta0 <- beta1
  }
  if (control$trace) {
    cat('Finished!\n')
  }
  
  # obtain the hessian
  M.obj <- Mstep(loglik, beta0, pi.ind, mu.ind, theta.ind, Z, X, M, Y, a1, offsetz, offsetx, offsetm, hessian=TRUE, 
                 control = list(trace = 0,
                                ndeps = rep(1e-5, length(beta0)),
                                maxit = 100,
                                REPORT = 10))
  
  fit <- M.obj
  fit$nIter <- nIter
  fit$loglik <- loglik0(beta1, pi.ind, mu.ind, theta.ind, Z, X, M, Y, offsetz, offsetx, offsetm)
  
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
  
  Yhat <- (1 - pi) * mu
  res <- sqrt(weights) * (Y - Yhat)
  nobs <- sum(weights > 0)
  rval <- list(coefficients = c(coefc, coefz, coefm), 
               estimates = list(mu = mu, pi = pi, theta = theta),
               residuals = res, fitted.values = Yhat, 
               optfit = fit, control = control, start = start, 
               weights = if (identical(as.vector(weights),rep.int(1L, n))) NULL else weights, 
               offset = list(count = if (identical(offsetx, rep.int(0, n))) NULL else offsetx, zero = if (identical(offsetz,rep.int(0, n))) NULL else offsetz, dispersion = if (identical(offsetm, rep.int(0, n))) NULL else offsetm), 
               n = nobs, df.null = nobs - 3, df.residual = nobs - (kx + kz + km), 
               terms = list(count = mtX, zero = mtZ, dispersion = mtM, full = mt),  
               loglik = fit$loglik,
               link = linkstr, linkinv = linkinv, converged = fit$converged, call = cl, formula = ff, 
               levels = .getXlevels(mt, mf), contrasts = list(count = attr(X, "contrasts"), zero = attr(Z, "contrasts"), dispersion = attr(M, "contrasts")))
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
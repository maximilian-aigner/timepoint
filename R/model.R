tpmodel <- function(cif, data, ilink = identity,
                    do.fit = TRUE, num_integral = TRUE,
                    ...) {
  stopifnot("eventlist" %in% class(data))
  if (is.character(cif)) {
    cif <- cif(cif)
  }
  m <- match.call()
  #mt.t <- terms(formula_times)
  #mt.m <- terms(formula_marks)
  if (do.fit) {
    pardim <- sum(unlist(cif$pars))
    
    nll <- \(par) -.lhood.marked(
      data$num_times,
      data$values,
      data$num_bounds[2],
      cif,
      par
    )
    
    out <- stats::optim(rep(0, pardim), fn = nll, ...)
  }
  else
    out <- NULL
  return(structure(list(
    data = data,
    type = type,
    mdens = mdens,
    covariates = covariates,
    fitted = out,
    ilink = ilink,
  ), class = "tpmodel"))
}

cif <- function(type, evalfun = NULL, compfun = NULL, dist = NULL) {
  if (type == "hpp") {
    pars <- list(lambda = 1)
    evalfun <- \(t,x,hist,par) par$lambda
    compfun <- \(t,hist,par) par$lambda*t
  } else if (type == "renewal") {
    stopifnot(!is.null(dist))
    densfun <- get(paste0("d", dist))
    cumfun <- get(paste0("p", dist))
    parnames <- setdiff(formalArgs(densfun), c("x", "log"))
    pars <- as.list(rep(1, length(parnames)))
    names(pars) <- parnames
    
    hfun <- function(x, ...) {
      densfun(x, ...) / cumfun(x, ..., lower.tail = FALSE)
    }
    evalfun <- \(t,x,hist,par) do.call(hfun,args = c(x = t - hist[nrow(hist),1],
                                              par))
    intfun <- function(t,hist,par) {
      diffs <- diff(c(0,hist[,1]))
      contribs <- sapply(diffs, \(tt) -do.call(cumfun, c(x = tt, par, lower.tail = FALSE, log = TRUE)))
      last_bit <- -do.call(cumfun, c(x = t - hist[nrow(hist),1], par, lower.tail = FALSE,
                                     log = TRUE))
      return(contribs + last_bit)
    }
  } else if (type == "scorr") {
    
  } else if (type == "hawkes") {
    
  } else {
    # Custom cif
    stopifnot(!is.null(evalfun) & !is.null(npar))
    if (is.null(compfun)) {
      # Use numerical integration:
      compfun = integrated(evalfun)
    }
  }
  
  return(structure(list(
    type = type,
    evalfun = evalfun,
    compfun = compfun,
    pars = pars
  )))
}

tpmodel.fit.poisson <- function(eventlist, covariates, ilink) {
  k <- length(covariates)
  th0 <- stats::runif(k, min = -1e-5, max = 1e-10)
  nt <- eventlist$num_times
  bds <- eventlist$num_bounds
  
  if (k == 1) {
    method <- "Brent"
    lower <- -999
    upper <- 999
  } else {
    method <- "Nelder-Mead"
    lower <- -Inf
    upper <- Inf
  }
  if (eventlist$marked) {
    xs <- eventlist$values
    ofun <- \(x) -.lhood.marked.poisson(nt, xs, bds, covariates, x)
  } else {
    ofun <- \(x)  -.lhood.unmarked.poisson(nt, bds, covariates, x)
  }
  result <- stats::optim(th0, fn = ofun, hessian = TRUE, method = method,
                         lower = lower, upper = upper)
  return(result)
}

.lhood.unmarked.poisson <- function(ntimes, bds, fs, th) {
  th <- as.matrix(th)
  evs <- sapply(fs, function(f) f(ntimes))
  term1 <- sum(evs %*% th)
  term2 <- stats::integrate(function(x, ...) {
    evals <- sapply(fs, \(f) f(x))
    res <- exp(evals%*%th)
    res[is.nan(res) | abs(res) > 1e20] <- 1e20
    return(res)
  },
  lower = bds[1],
  upper = bds[2],
  stop.on.error = FALSE
  )$value

  return(term1-term2)
}

.lhood.marked.poisson <- function(ntimes, xs, bds, fs, th) {
  th <- as.matrix(th)
  print(dim(th))
  
  evs <- sapply(fs, function(f) f(ntimes, xs)) # N times K (f_k(t_i, x_i))
  print(dim(evs))
  term1 <- sum(evs %*% th)
  
  term2 <- cubature::adaptIntegrate(function(x) {
    evals <- sapply(fs, \(f) f(x[1], x[2:length(x)])) # N times K
    res <- exp(evals%*%th)
    res[is.nan(res) | abs(res) > 1e20] <- 1e20
    return(res)
  }, lowerLimit = bds[c(1,3)], upperLimit = bds[c(2,4)])$integral
  return(term1 - term2)
}

.lhood.marked <- function(num_times, marks, bT, cif, par) {
  # hists <- sapply(1:length(num_times))
  histories <- lapply(1:length(num_times), function(i) cbind(num_times[1:i], marks[1:i]))
  evals_lg <- mapply(num_times, marks, histories, \(t,x,hist) cif$evalfun(t, x, hist, par))
  # evals_mdens <- mapply(marks, num_times, mdens)
  
  int_lg <- cif$compfun(bT,histories[length(num_times)], par) # - cif$compfun(0)
  return(sum(log(evals_lg)) - int_lg)
}

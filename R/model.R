tpmodel <- function(eventlist, covariates, ilink = exp, do.fit = TRUE,
                    type = "poisson") {
  stopifnot(type == "poisson")
  if (do.fit) {
    if (type == "poisson")
      out <- tpmodel.fit.poisson(eventlist, covariates, ilink)
  }
  else
    out <- NULL
  return(structure(list(
    eventlist = eventlist,
    covariates = covariates,
    fitted_model = out,
    ilink = ilink,
    unit = eventlist$num_unit
  ), class = "tpmodel"))
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
  result <- stats::optim(th0, fn = \(x) -.lhood.unmarked.poisson(nt, bds, covariates, x), control = list(trace = 1),
                  hessian = TRUE, method = method, lower = lower, upper = upper)
  
  # Result is good but on numeric scale,
  # i.e. it matches length(eventlist)/eventlist$num_bounds[2]
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

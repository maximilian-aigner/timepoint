# Simulation scripts
exp_kernel <- function(x, a = 1) {
  exp(-a*x)
}

sim_hop <- function(bounds = c(0, 100),
                           rate = 1) {
  number <- diff(bounds)*rate
  pts <- sort(runif(number, bounds[1], bounds[2]))
  return(pts)
}

# Heuristic for kernel bound
.kernel.bound <- function(kernel, bds, n = 100) {
    # estimate upper bound conservatively by discretisation
  ts <- seq(bds[1], bds[2],len=n)
  kdisc <- kernel(ts)
  kernel_bound <- sqrt(n) * max(kdisc)
  #res <- filter(ts, kdisc, sides = 1)
  #kernel_bound <- max(res, na.rm = TRUE) * 2
  return(kernel_bound)
}

.thinning.alg <- function(lambda, ub, bT, ap = FALSE) {
  v <- numeric(length = 0)
  tt <- 0
  acs <- v
  while(tt<bT) {
    s <- rexp(1, rate = ub)
    tt <- tt + s
    u <- runif(1)
    tv <- lambda(tt)/ub
    acs <- c(acs, tv)
    stopifnot("thinning upper bound exceeded" = tv < 1)
    #print(tv)
    if (u < tv | is.infinite(tv)) {
      v <- c(v, tt)
    }
  }
  if (ap) {
    return(list(v=v, ap = mean(acs)))
  }
  return(v)
}


sim_cluster_poisson <- function(intervention_points,
                         bounds = c(0, 10),
                         kernel = exp_kernel,
                         kernel_bound = NULL) {
  if (is.null(kernel_bound)) {
    kernel_bound <- .kernel.bound(kernel, bounds)
  }
  intens_upper_bound <- 2* kernel_bound
  lambda <- function(x) {
    prev <- intervention_points[intervention_points<x]
    if(length(prev)==0) return(0)
    v <- sapply(prev, \(u) kernel(x-u))
    return(sum(v))
  }
  pts <- .thinning.alg(lambda, ub = intens_upper_bound, bT = diff(bounds))
  pts <- bounds[1] + pts
  pts <- pts[pts<bounds[2]]
  return(list(intervention_points = intervention_points, observed = pts))
}

sim_dsp <- function(bounds = c(0, 10),
                         rate = 1,
                         kernel = exp_kernel,
                         kernel_bound = NULL) {
  latent_pts <- sim_hop(bounds, rate)
  obs_pts <- sim_cluster_poisson(latent_pts, bounds = kernel, kernel_bound)
  return(list(latent = latent_pts, observed = obs_pts))
}

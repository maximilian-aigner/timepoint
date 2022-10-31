# Simulation scripts
exp_kernel <- function(x, a = 1) {
  exp(-a*x)
}

sim_hop <- function(bounds = c(0, 100),
                           rate = 1) {
  number <- rpois(1, diff(bounds)*rate)
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
  obs_pts <- sim_cluster_poisson(latent_pts, kernel = kernel, bounds = bounds,
                                 kernel_bound)
  return(list(latent = latent_pts, observed = obs_pts$observed))
}

sim_ihop <- function(lambda, bounds) {
  ub <- optimise(lambda, interval = bounds, maximum = TRUE)$maximum
  .thinning.alg(lambda, ub, bounds[2], ap = FALSE)
}

sim_gw <- function(pts, rkernel,bounds) {
  end<-bounds[2]
  generate_cluster = function(origin, max.generations = 100) {
    cluster = vector("list", max.generations)
    cluster[[1]] = list(times = origin, parents = 0)
    
    for (n in 1:max.generations) {
      if (n == max.generations) warning("Hit max generations")
      
      new_parents <- c()
      new_times <- c()
      for (point in cluster[[n]]$times) {
        # Lewis' thinning algorithm
        s <- 0
        ubound <- 3*rkernel(0)
        offspring = c()
        while(point + s < end) {
          s = s + rexp(1, ubound)
          d = runif(1)
          if(d * ubound <= rkernel(s)) {
            offspring = c(offspring, s)
          }
        }
        new_parents <- c(new_parents, rep(point, length(offspring)))
        new_times <- c(new_times, point + offspring)
      }
      if (length(new_times) == 0) # no new points, the cluster has died out
        break
      else {
        cluster[[n+1]] = list(times = new_times,
                              parents = new_parents)
      }
    }
    return(cluster[!sapply(cluster, is.null)])
  }
  
  clusters <- lapply(pts, generate_cluster)
  
  times <- unlist(lapply(unlist(clusters, recursive = FALSE), `[[`, "times"))
  parents <- unlist(lapply(unlist(clusters, recursive = FALSE), `[[`, "parents"))
  return(list(times=times,parents=parents))
}

sim_arma <- function(mu, theta, phi, bounds) {
  stage_1 <- sim_hop(bounds, rate = mu)
  stage_2 <- sim_cluster_poisson(intervention_points = stage_1,
                                 bounds = bounds, kernel = theta)
  stage_3 <- sim_gw(stage_2$observed, rkernel = phi, bounds = bounds)
  return(list(immigr = stage_1, ma = stage_2, ar = stage_3))
}

sim_hawkes <- function(eta, phi, bounds) {
  bg <- sim_hop(bounds, rate = eta)
  excited <- sim_gw(pts = bg, rkernel = phi, bounds = bounds)
  return(list(bg=bg,excited=excited$times))
}
cov.brill <- function(eventlist, lag.max = NULL, bw = NULL, debias = TRUE,
                      plot = TRUE, ...) {
  obs <- eventlist$num_times
  bT <- eventlist$num_bounds[2]
  if (is.null(lag.max)) lag.max <- bT
  if (is.null(bw)) bw <- bT / 100
  u <- seq(0-bw/2, lag.max+bw/2, by=bw)
  iat <- diff(eventlist$num_times)
  iat.hist <- hist(iat[iat < lag.max], breaks = u, plot = FALSE)
  lambdabar <- length(eventlist) / bT
  emp.cov <- iat.hist$counts / (bw * bT)  - lambdabar^2 / bT# bT#/ length(obs)
  #emp.cov[1] <- emp.cov[1] - lambdabar^2
  if (debias) {
    emp.cov <- emp.cov + u[-length(u)] * lambdabar^2 / bT
  }
  if (plot) {
    plot(iat.hist$mids, emp.cov, type = 'l', ...)
  }
  return(list(t=iat.hist$mids,ct = emp.cov))
}

pgram.pp <- function(eventlist, end = NULL, nf = 256, demean = TRUE, plot = TRUE, ...) {
  N <- length(eventlist)
  lambdabar <- length(eventlist) / eventlist$num_bounds[2]
  bT <- end
  if (is.null(bT)) {
    bT <- eventlist$num_bounds[2]
  }
  f.vals <-  seq(0, pi/2, length.out = nf)
  J <- sqrt(2/bT) * rowSums(exp(1i*f.vals%*%t(eventlist$num_times)))
  if (demean) {
    J <- J - sqrt(2/bT) * 1/N * rowSums(exp(1i*f.vals%*%t(rep(lambdabar, N))))
  }
  if (plot) {
    plot(f.vals, Mod(J), type = 'l', ...)
  }
  return(list(f.vals=f.vals, spec=Mod(J)))
}
apgram.pp <- function(eventlist, K = 10, nf = 256, plot = TRUE, ...) {
  splitted <- folds(eventlist, K)
  f.vals <-  seq(0, pi/2, length.out = nf)
  averaged <- Mod(rowMeans(sapply(splitted, \(x) pgram.pp(x, ...)$spec)))
  if (plot) {
    plot(f.vals, averaged)
  }
  return(list(f.vals=f.vals, spec=averaged))
}

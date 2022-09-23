# Various utility functions, e.g. for transforming datasets


# Adapted from BioConductor
invert_list <- function(l) {
  if (length(l) == 0) return(l)
  lens <- lengths(l)
  nams <- rep(names(l), lens)
  vals <- unlist(l)
  split(nams, vals)
}

# data should be a matrix with columns corresponding to different individuals
# and entries situating them in a given location
presence_to_transitions <- function(data, times = NULL) {
  if (!is.matrix(data) & !is.data.frame(data)) {
    warning("coercing presence data to matrix")
    data <- as.matrix(data)
  }
  ind.names <- names(data)
  if (is.null(ind.names)) ind.names <- 1:ncol(data)
  by.individual <- lapply(1:ncol(data), function(j) {
    one.series <- data[,j]
    rl <- rle(one.series)
    origin <- c(NA, rl$values)
    transfer_time <- c(NA, cumsum(rl$lengths))
    destination <- c(rl$values, NA)
    id <- rep(ind.names[j], length(origin))
    return(cbind(id,origin,destination,transfer_time))
  })
  names(by.individual) <- ind.names
  out <- as.data.frame(do.call("rbind", by.individual))
  if (!is.null(times)) {
    out$transfer_time <- times[out$transfer_time]
  }
  return(out)
}

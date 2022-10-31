intensity <- function(el, const = FALSE, ...) {
  mc <- match.call(expand.dots = TRUE)
  num_bw <- "nrd0" # hack
  if (!is.null(mc$bw)) {
    if (is.character(mc$bw)) {
      # check if one of special bw algorithm strings
      parsed <- as.difftime(lubridate::period(mc$bw))
      if (is.na(parsed)) {
        num_bw <- mc$bw
      } else {
        units(parsed) <- el$num_unit
        num_bw <- as.numeric(parsed)
      }
    } else {
      num_bw <- mc$bw
    }
  }
  fn <- ifelse(const, .const.intensity, .intensity)
  return(fn(x = el$num_times, bT = el$num_bounds[2], bw = num_bw))
}

.intensity <- function(x, bT, ...) {
  dens <- suppressWarnings(density(x, ..., weights = rep(1,length(x)),
                                   from = 0, to = bT))
  dens$integral <- length(x)
  dens$call <- str2lang("intensity(x, ...)")
  return(dens)
}

.const.intensity <- function(x, bT, ...) {
  return(length(x)/bT)
}
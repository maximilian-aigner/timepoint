# Constructor
new_eventlist_from_dt <- function(datetimes, values = NULL,
                                  bounds = NULL, units = "auto") {
  if (!is.null(values)) {
    stopifnot(length(datetimes) == length(values)) 
  }
  if (!is.null(bounds))
  {
    stopifnot(length(bounds) != 2 | is.POSIXt(bounds))
  } else {
    warning("No bounds provided, using data range")
    bounds <- range(datetimes)
  }
  
  stopifnot(is.POSIXt(datetimes))
  dts <- difftime(time1 = datetimes,
                  time2 = rep(bounds[1], length(datetimes)),
                  units = units)
  
  units <- attr(dts, "units")
  
  possible_units <- c("secs", "mins", "hours",
                      "days", "weeks")
  
  # Reduce numtimes for numerical stability (...)
  while(any(as.numeric(dts) > 1000)) {
    if (units != "weeks") { # Otherwise, cannot increase
      new_units <- possible_units[which(units == possible_units) + 1]
      units(dts) <- new_units
      units <- new_units
    } else {
      break
    }
  }
  
  num_length <- difftime(bounds[2], bounds[1], units = units)
  
  structure(list(
    datetimes = datetimes,
    num_times = as.numeric(dts),
    num_unit = units,
    bounds = bounds,
    num_bounds = c(0, as.numeric(num_length)),
    values = values,
    marked = !is.null(values)
  ), class = "eventlist")
}

# Helper constructor
eventlist <- function(datetimes, values = NULL,
                      bounds = NULL, units = "auto") {
  # TODO: Implement creation from other types? File descriptors?
  new_eventlist_from_dt(datetimes, values, bounds, units)
}

summary.eventlist <- function(object, ...) {
  cat("Event list spanning ")
  cat(as.character(object$bounds[1]))
  cat(" -- ")
  cat(as.character(object$bounds[2]))
  cat(" with ")
  cat(length(object$datetimes))
  cat(" events")
  if (object$marked) {
    cat(", marked")
  }
}

length.eventlist <- function(x) {
  return(length(x$datetimes))
}

`[.eventlist` <- function(el, i, j = NULL, type = "dt") {
  if (el$marked) {
    rval <- el$values[i]
  } else {
    rval <- NULL
  }
  if (type == "dt")
    return(list(datetimes=el$datetimes[i], values = rval))
  else
    return(list(num_times=el$num_times[i], values = rval))
}

folds <- function(el, K) {
  intervals <- seq.POSIXt(el$bounds[1], el$bounds[2],
                          length.out = K + 1)
  folds <- lapply(1:K, function(j) {
    indices <- el$datetimes > intervals[j] &
      el$datetimes < intervals[j+1]
    chunk <- new_eventlist_from_dt(
      datetimes = el$datetimes[indices],
      values = if (el$marked) el$values[indices] else NULL,
      bounds = intervals[j:(j+1)]
    )
  })
}
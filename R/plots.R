# Plotting functions for eventlist and tpmodel objects
# Maximilian Aigner

# "Bar code" plot
bcplot <- function(el, add = FALSE, ...) {
  if (!add) {
    plot.new()
    plot.window(xlim = el$bounds, ylim = c(0, 1))
  }
  segments(x0 = el$datetimes, y0 = rep(0, length(el$datetimes)),
           y1 = rep(1, length(el$datetimes)), ...)
  if (!add) {
    axis.POSIXct(x = el$datetimes, side = 1, cex.axis = 0.8)
    axis.POSIXct(at = el$bounds, side = 1, line = 2, lwd = 0, format = "%Y-%m-%d %H:%M:%S")
    title(main = paste("Events from", paste0(el$bounds, collapse = "--"), "\n"), cex.main = 1)
  }
}

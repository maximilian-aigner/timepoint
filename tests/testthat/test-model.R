library(lubridate)
set.seed(3)
lambda_true <- 5
bT <- 100 # days

n_events <- rpois(1, lambda_true * bT)
times <- stats::runif(n_events, min = 0, max = bT)

origin <- ymd_hms("2022-01-01 00:00:00")
end <- origin + seconds(100 * 24 * 60 * 60)
datetimes <- origin + seconds(times*24*60*60)

el <- eventlist(datetimes, bounds = c(origin, end))

cova <- list(\(x) rep(1, length(x)))#, \(x) x / 1000)

test_that("converges_on_constant", {
  fit <- tpmodel(el, cova)
  empirical <- length(el) / (bT * 24)
  optimised <- exp(fit$fitted_model$par)
  expect_lt(abs(optimised - empirical), expected = 0.05)
})
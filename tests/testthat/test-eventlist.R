library(lubridate)

# start <- lubridate::ymd_hms("2022-01-01 00:00:00")
# end <- lubridate::ymd_hms("2022-01-01 23:59:59")
events <- lubridate::ymd_hms("2022-01-01 06:00:00", "2022-01-01 23:59:59")
genders <- list(c(gender="male"), c(gender="female"))
el <- eventlist(datetimes = events,
                values = genders,
                bounds = ymd_hms("2022-01-01 00:00:00", "2022-01-01 23:59:59"),
                units = "secs")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("num_makes_sense", {
  # should internally store time as hours
  expect_equal(el$num_times[1], 6 * 60 * 60)
  expect_equal(el$num_times[2], 24 * 60 * 60 - 1)
})

test_that("subsetting_works", {
  expect_equal(as.character(el[1]$values), "male")
})
test_state_data <- cbind(c(1,1,3,3), c(2,2,2,2), c(2,1,3,1), c(2,2,1,2))

test_that("transfers_formed_correctly", {
  simplest <- rep(c(0,1), times = c(9, 1))
  transitions <- presence_to_transitions(as.matrix(simplest))
  expect_equal(transitions[2,"origin"], 0)
  expect_equal(transitions[2,"destination"], 1)
})
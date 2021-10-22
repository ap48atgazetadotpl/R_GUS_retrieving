

# Testing function "retrieve_gus()" throughout following expectations - throwing message when no argument

test_that("checking if error is detected", {
  expect_error(retrieve_gus())
})


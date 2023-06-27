#-------------------------------------------------------------------------------
# SUMMARISER
#-------------------------------------------------------------------------------

test_that("summariseR", {
  # Test case 1: Valid input
  test1 <- c(rep(1, 305), rep(0, 95), rep(1, 50), rep(0, 150))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, gold)
  expect_no_error(summariseR(df))

  # Test case 2: Invalid input
  test1 <- c(rep(1, 305), rep(0, 95), rep(1, 50), rep(0, 150))
  test2 <- c(rep(1, 303), rep(0, 97), rep(1, 60), rep(0, 140))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  expect_error(summariseR(df))
})

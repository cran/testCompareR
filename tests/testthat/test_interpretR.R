#-------------------------------------------------------------------------------
# INTERPRETR
#-------------------------------------------------------------------------------

test_that("interpretR", {
  # Test 1: Valid argument

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)

  result <- compareR(df)

  expect_no_error(interpretR(result))

  # Test 2: Invalid argument

  result <- "lemon"

  expect_error(interpretR(result))

  # Test 3: Tests have exactly equal, very poor performance

  test1 <- c(rep(1, 10), rep(0, 390), rep(1, 190), rep(0, 10))
  test2 <- c(rep(1, 10), rep(0, 390), rep(1, 190), rep(0, 10))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)

  result <- compareR(df)

  expect_no_error(interpretR(result))
})

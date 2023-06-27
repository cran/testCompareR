#-------------------------------------------------------------------------------
# DATAFRAMER
#-------------------------------------------------------------------------------

test_that("dataframeR", {
  # Test case 1: Valid digits
  expect_no_error(dataframeR(3, 3, 3, 3, 3, 3, 3, 3))

  # Test case 2: Input of decimal values
  expect_error(dataframeR(3.1, 3, 3, 3, 3, 3, 3, 3))
})

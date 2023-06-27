#-------------------------------------------------------------------------------
# PRODUCING A DATA FRAME FROM PUBLISHED DATA
#-------------------------------------------------------------------------------

#' @title dataframeR
#'
#' @description Produces a data frame which can be used by the compareR function
#' using values commonly found in published literature. Useful for reviews and
#' meta-analyses.
#'
#' @param s11 Number of cases where Test 1 is positive, Test 2 is positive and
#' gold standard is positive.
#' @param s10 Number of cases where Test 1 is positive, Test 2 is negative and
#' gold standard is positive.
#' @param s01 Number of cases where Test 1 is negative, Test 2 is positive and
#' gold standard is positive.
#' @param s00 Number of cases where Test 1 is negative, Test 2 is negative and
#' gold standard is positive.
#' @param r11 Number of cases where Test 1 is positive, Test 2 is positive and
#' gold standard is negative.
#' @param r10 Number of cases where Test 1 is positive, Test 2 is negative and
#' gold standard is negative.
#' @param r01 Number of cases where Test 1 is negative, Test 2 is positive and
#' gold standard is negative.
#' @param r00 Number of cases where Test 1 is negative, Test 2 is negative and
#' gold standard is negative.
#'
#' @return A data frame populated with zeros and ones indicating positive or
#' negative test results which can be passed to the compareR function.
#'
#' @details
#' Understanding the parameter names:
#' s & r represent positive and negative results for the gold standard test,
#' respectively.
#' The first digit represents a positive (1) or negative (0) result for Test 1.
#' The second digit represents a positive (1) or negative (0) result for Test 2.
#'
#' @examples
#' # build data frame using numbers
#' dataframeR(3, 3, 3, 3, 3, 3, 3, 3)
#'
#' @export

dataframeR <- function(s11, s10, s01, s00, r11, r10, r01, r00) {
  vect <- c(s11, s10, s01, s00, r11, r10, r01, r00)

  vect <- vect %% 1
  for (i in seq_along(vect)) {
    if (vect[i] > 0) {
      stop("Input values must be whole numbers")
    }
  }

  test1 <- c(
    rep(1, s11), rep(1, s10), rep(0, s01), rep(0, s00),
    rep(1, r11), rep(1, r10), rep(0, r01), rep(0, r00)
  )

  test2 <- c(
    rep(1, s11), rep(0, s10), rep(1, s01), rep(0, s00),
    rep(1, r11), rep(0, r10), rep(1, r01), rep(0, r00)
  )

  gold <- c(rep(1, (s11 + s10 + s01 + s00)), rep(0, (r11 + r10 + r01 + r00)))

  data <- data.frame(test1, test2, gold)

  return(data)
}

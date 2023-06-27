#-------------------------------------------------------------------------------
# SUMMARISE ONE BINARY DIAGNOSTIC TEST IN A SINGLE FUNCTION CALL
#-------------------------------------------------------------------------------

#' @title summariseR
#'
#' @description Summarises descriptive statistics associated with a single
#' binary diagnostic test.
#'
#' @param df A data frame or matrix with 2 columns (test1, gold). Flexible
#' coding of positive and negative results permitted.
#' @param dp Number of decimal places of output in summary tables. Defaults
#' to 1. Kappa defaults to 3 decimal places unless user selects more.
#'
#' @return A summary of the descriptive statistics of a binary diagnostic test,
#' compared to a gold standard.
#'
#' @details
#' Confidence intervals for prevalence, diagnostic accuracies and predictive
#' values are calculated using the interval for binomial proportions described
#' by Yu et al. (2014).
#' Confidence intervals for likelihood ratios are calculated using the methods
#' recommended by Martín-Andrés and Álvarez-Hernández (2014).
#' Cohen's kappa is a value between -1 and 1 which describes the agreement of
#' the two tests, taking account of random agreement. A score of zero or less
#' indicates the agreement could be entirely due to chance.
#'
#' @references
#' Yu, Guo & Xu (2014) JSCS. 2014; 84:5,1022-1038
#' \doi{10.1080/00949655.2012.738211}
#'
#' Martín Andrés & Álvarez Hernández (2014) Stat Comput. 2014; 24,65–75
#' \doi{10.1007/s11222-012-9353-5}
#'
#' Cohen (1960) Educ Psychol Meas. 1960; 20(1),37–46
#' \doi{10.1177/001316446002000104}
#'
#' @examples
#' # simulate data
#' test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
#' gold <- c(rep(1, 400), rep(0, 200))
#' dat <- data.frame(test1, gold)
#'
#' # summarise descriptive statistics
#' result <- summariseR(dat, dp = 4)
#'
#' @export

summariseR <- function(df, dp = 1) {
  if (ncol(df) != 2) {
    stop("Please provide data as a data frame with two columns.")
  }

  df <- recoder(df)

  vals <- values.1test(df)

  cont <- disp.cont(vals)

  prev <- matrixify(conf.prev(vals), dp = dp + 2)

  acc <- matrixify(conf.acc(vals),
    rows = c("Sensitivity", "Specificity"), dp = dp + 2
  )

  pv <- matrixify(conf.pv(vals), rows = c("PPV", "NPV"), dp = dp + 2)

  lr <- matrixify(conf.lr(vals), rows = c("PLR", "NLR"), dp = dp + 2)

  po <- (vals$s1 + vals$r0) / vals$n
  pe <- ((vals$s1 + vals$s0) / vals$n * (vals$s1 + vals$r1) / vals$n) +
    ((vals$r0 + vals$s0) / vals$n * (vals$r0 + vals$r1) / vals$n)
  kappa <- (po - pe) / (1 - pe)
  if (dp < 3) {
    kappa <- round(kappa, 3)
  } else {
    kappa <- round(kappa, dp)
  }

  out <- list(cont = cont,
              prev = prev,
              acc = acc,
              pv = pv,
              lr = lr,
              kappa = kappa)

  class(out) = "summariseR"

  return(out)

}

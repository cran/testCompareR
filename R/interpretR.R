#-------------------------------------------------------------------------------
# INTERPRET THE RESULTS OF COMPARER
#-------------------------------------------------------------------------------

#' @title interpretR
#'
#' @description Provides a plain English readout of the results of the compareR
#' function.
#'
#' @param result A list object with class 'compareR' output from the compareR
#' function.
#'
#' @return A plain English summary of the findings produced by the compareR
#' function.
#'
#' @examples
#' # simulate data
#' test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
#' test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
#' gold <- c(rep(1, 400), rep(0, 200))
#' dat <- data.frame(test1, test2, gold)
#'
#' # compare with compareR
#' result <- compareR(dat)
#'
#' # provide a plain English readout with interpretR
#' interpretR(result)
#'
#' @export

interpretR <- function(result) {
  ## check arguments

  if (inherits(result, "compareR")) {
    ## warnings about data quality issues that may affect results

    if (result$other$equal == TRUE | result$other$zeros > 0 |
        result$other$Youden1 <= 0 | result$other$Youden1 <= 0) {
      cat("\nWARNING:\n")
      if (result$other$equal == TRUE) {
        cat("Tests have exactly equal performance. Tests may return NA/NaN.\n")
      }
      if (result$other$zeros > 0) {
        cat("Zeros exist in contingency table. Tests may return NA/NaN.\n")
      }
      if (result$other$Youden1 <= 0) {
        cat("Youden Index of ", result$other$test.names[1], " is 0. Tests may return NA/NaN.\n")
      }
      if (result$other$Youden2 <= 0) {
        cat("Youden Index of ", result$other$test.names[2], " is 0. Tests may return NA/NaN.\n")
      }
      if (result$other$Youden1 <= 0 | result$other$Youden2 <= 0) {
        cat("When Youden Index is less than or equal to zero a test returns the \nsame proportion of positive results irrespective of true status.\nThis indicates poor test performance. ")
      }
    }

    alpha <- result$other$alpha
    test.names <- result$other$test.names

    ## contingency tables

    cat("\n--------------------------------------------------------------------------------")
    cat("\nCONTINGENCY TABLES")
    cat("\n--------------------------------------------------------------------------------")
    cat("\n\nTrue Status - POSITIVE\n")

    print(result$cont$`True Status: POS`)

    cat("\nTrue Status - NEGATIVE\n")

    print(result$cont$`True Status: NEG`)

    cat("\nGold standard vs. Test 1\n")

    print(result$cont$`Gold standard vs. Test 1`)

    cat("\nGold standard vs. Test 2\n")

    print(result$cont$`Gold standard vs. Test 2`)

    ## prevalence

    cat("\n--------------------------------------------------------------------------------")
    cat("\nPREVALENCE (%)")
    cat("\n--------------------------------------------------------------------------------\n\n")

    print(result$prev)

    ## descriptive plus inferential statistics for diagnostic accuracies

    if (!is.null(result$acc)) {
      cat("\n--------------------------------------------------------------------------------")
      cat("\nDIAGNOSTIC ACCURACIES")
      cat("\n--------------------------------------------------------------------------------")

      cat("\n\n", test.names[1], "(%)\n") # add test names to output of compareR to have user input names

      print(result$acc$accuracies[[1]])

      cat("\n", test.names[2], "(%)\n") # add test names to output of compareR to have user input names

      print(result$acc$accuracies[[2]])

      if (is.na(result$acc$glob.p.adj)) {
        cat("\nGlobal hypothesis testing was not or could not be performed.\nThis is usually because:\n* prevalence is <10% and total number of participants < 100\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests. Individual tests have been \nattempted below.")
      } else {
        cat("\nGlobal Null Hypothesis: Se1 = Se2 & Sp1 = Sp2\n")
        cat("Test statistic: ", result$acc$glob.test.stat, " Adjusted p value: ", result$acc$glob.p.adj)
        if (result$acc$glob.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
      }
      if (is.na(result$acc$glob.p.adj) | result$acc$glob.p.adj < alpha) {
        cat("\n\nInvestigating individual differences\n\n")
        cat("Null Hypothesis 1: Se1 = Se2\n")
        cat("Test statistic: ", result$acc$sens.test.stat, " Adjusted p value: ", result$acc$sens.p.adj)
        if (!is.na(result$acc$sens.p.adj) && result$acc$sens.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
        if (is.na(result$acc$sens.test.stat) | is.na(result$acc$sens.p.adj)) {
          cat("\nNAs or NaNs in individual tests.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests.")
        }
        cat("\n\nNull Hypothesis 2: Sp1 = Sp2\n")
        cat("Test statistic: ", result$acc$spec.test.stat, " Adjusted p value: ", result$acc$spec.p.adj)
        if (!is.na(result$acc$spec.p.adj) && result$acc$spec.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
        if (is.na(result$acc$spec.test.stat) | is.na(result$acc$spec.p.adj)) {
          cat("\nNAs or NaNs in individual tests.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests.")
        }
      } else {
        cat("\nDo not reject the global null hypothesis. Difference in diagnostic accuracies is NOT significant.")
      }
    }

    ## descriptive plus inferential statistics for predictive values

    if (!is.null(result$pv)) {
      cat("\n\n--------------------------------------------------------------------------------")
      cat("\nPREDICTIVE VALUES")
      cat("\n--------------------------------------------------------------------------------")

      cat("\n\n", test.names[1], "(%)\n") # add test names to output of compareR to have user input names

      print(result$pv$predictive.values[[1]])

      cat("\n", test.names[2], "(%)\n") # add test names to output of compareR to have user input names

      print(result$pv$predictive.values[[2]])

      if (is.na(result$pv$glob.p.adj)) {
        cat("\nGlobal hypothesis testing was not or could not be performed.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests. Individual tests have been \nattempted below.")
      } else {
        cat("\nGlobal Null Hypothesis: PPV1 = PPV2 & NPV1 = NPV2\n")
        cat("Test statistic: ", result$pv$glob.test.stat, " Adjusted p value: ", result$pv$glob.p.adj)
        if (result$pv$glob.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
      }
      if (is.na(result$pv$glob.p.adj) | result$pv$glob.p.adj < alpha) {
        cat("\n\nInvestigating individual differences\n\n")
        cat("Null Hypothesis 1: PPV1 = PPV2\n")
        cat("Test statistic: ", result$pv$ppv.test.stat, " Adjusted p value: ", result$pv$ppv.p.adj)
        if (!is.na(result$pv$ppv.p.adj) && result$pv$ppv.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
        if (is.na(result$pv$ppv.test.stat) | is.na(result$pv$ppv.p.adj)) {
          cat("\nNAs or NaNs in individual tests.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests.")
        }
        cat("\n\nNull Hypothesis 2: NPV1 = NPV2\n")
        cat("Test statistic: ", result$pv$npv.test.stat, " Adjusted p value: ", result$pv$npv.p.adj)
        if (!is.na(result$pv$npv.p.adj) && result$pv$npv.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
        if (is.na(result$pv$npv.test.stat) | is.na(result$pv$npv.p.adj)) {
          cat("\nNAs or NaNs in individual tests.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests.")
        }
      } else {
        cat("\nDo not reject the global null hypothesis. Difference in predictive values is NOT significant.")
      }
    }

    ## descriptive plus inferential statistics for likelihood ratios

    if (!is.null(result$lr)) {
      cat("\n\n--------------------------------------------------------------------------------")
      cat("\nLIKELIHOOD RATIOS")
      cat("\n--------------------------------------------------------------------------------")

      cat("\n\n", test.names[1], "(%)\n") # add test names to output of compareR to have user input names -> not currently working

      print(result$lr$likelihood.ratios[[1]])

      cat("\n", test.names[2], "(%)\n") # add test names to output of compareR to have user input names -> not currently working

      print(result$lr$likelihood.ratios[[2]])

      if (is.na(result$lr$glob.p.adj)) {
        cat("\nGlobal hypothesis testing was not or could not be performed.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests. Individual tests have been \nattempted below.")
      } else {
        cat("\nGlobal Null Hypothesis: PLR1 = PLR2 & NLR1 = NLR2\n")
        cat("Test statistic: ", result$lr$glob.test.stat, " Adjusted p value: ", result$lr$glob.p.adj)
        if (result$lr$glob.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
      }
      if (is.na(result$lr$glob.p.adj) | result$lr$glob.p.adj < alpha) {
        cat("\n\nInvestigating individual differences\n\n")
        cat("Null Hypothesis 1: PLR1 = PLR2\n")
        cat("Test statistic: ", result$lr$plr.test.stat, " Adjusted p value: ", result$lr$plr.p.adj)
        if (!is.na(result$lr$plr.p.adj) && result$lr$plr.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
        if (is.na(result$lr$plr.test.stat) | is.na(result$lr$plr.p.adj)) {
          cat("\nNAs or NaNs in individual tests.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests.")
        }
        cat("\n\nNull Hypothesis 2: NLR1 = NLR2\n")
        cat("Test statistic: ", result$lr$nlr.test.stat, " Adjusted p value: ", result$lr$nlr.p.adj)
        if (!is.na(result$lr$nlr.p.adj) && result$lr$nlr.p.adj < alpha) {
          cat(" ***SIGNIFICANT***")
        }
        if (is.na(result$lr$nlr.test.stat) | is.na(result$lr$nlr.p.adj)) {
          cat("\nNAs or NaNs in individual tests.\n\nThis is usually because:\n* tests have identical performance\n* there are many zeros in the contingency table\n* Youden Index is < 0\n\nConsider the quality of your data and/or tests.")
        }
      } else {
        cat("\nDo not reject the global null hypothesis. Difference in likelihood ratios is NOT significant.")
      }
    }
  } else if (inherits(result, "summariseR")) {

    cat("\n--------------------------------------------------------------------------------")
    cat("\nCONTINGENCY TABLES")
    cat("\n--------------------------------------------------------------------------------\n\n")

    print(result$cont)

    cat("\n--------------------------------------------------------------------------------")
    cat("\nPREVALENCE (%)")
    cat("\n--------------------------------------------------------------------------------\n\n")

    print(result$prev)

    cat("\n--------------------------------------------------------------------------------")
    cat("\nDIAGNOSTIC ACCURACIES")
    cat("\n--------------------------------------------------------------------------------\n\n")

    print(result$acc)

    cat("\n--------------------------------------------------------------------------------")
    cat("\nPREDICTIVE VALUES")
    cat("\n--------------------------------------------------------------------------------\n\n")

    print(result$pv)

    cat("\n--------------------------------------------------------------------------------")
    cat("\nLIKELIHOOD RATIOS")
    cat("\n--------------------------------------------------------------------------------\n\n")

    print(result$lr)

    cat("\n--------------------------------------------------------------------------------")
    cat("\nCOHEN'S KAPPA")
    cat("\n--------------------------------------------------------------------------------\n\n")

    cat(result$kappa)

    cat("\n")

  } else {
    stop("The interpretR() function can only interpret output from the compareR() or summariseR() functions.")
  }

}

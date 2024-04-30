#-------------------------------------------------------------------------------
# COMPARE TWO BINARY DIAGNOSTIC TESTS IN A SINGLE FUNCTION CALL
#-------------------------------------------------------------------------------

#' @title compareR
#'
#' @description Calculates descriptive statistics and performs statistical
#' inference on two binary diagnostic tests in a single function call. Handles
#' multiple comparisons using methods in `p.adjust()`.
#'
#' @param df A data frame or matrix with 3 columns (test1, test2, gold).
#' Flexible coding of positive and negative results permitted.
#' @param alpha An alpha value. Defaults to 0.05.
#' @param margins A Boolean value indicating whether the contingency tables
#' should have margins containing summed totals of rows and columns.
#' @param multi_corr Method for multiple comparisons. Uses `p.adjust.methods`.
#' @param cc A Boolean value indicating whether McNemar's test should be applied
#' with continuity correction.
#' @param dp Number of decimal places of output in summary tables.
#' Defaults to 1.
#' @param sesp A Boolean value indicating whether output should include
#' sensitivity and specificity.
#' @param ppvnpv A Boolean value indicating whether output should include
#' positive and negative predictive values.
#' @param plrnlr A Boolean value indicating whether output should include
#' positive and negative likelihood ratios.
#' @param conf.int A character string, either "contemporary" or "classic".
#' Indicates whether function should use contemporary or classic statistical
#' methods to calculate confidence intervals.
#' @param test.names A vector of length two giving the names of the two
#' different binary diagnostic tests. This argument is not relevant when testing
#' a single binary diagnostic test.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object summarising all calculated descriptive and
#' inferential statistics.
#'
#' @details
#' Confidence intervals for prevalence, diagnostic accuracies and predictive
#' values are calculated using the interval for binomial proportions described
#' by Yu et al. (2014) by default. Setting conf.int = "classic" uses the
#' Clopper-Pearson method.
#' Confidence intervals for likelihood ratios are calculated using the methods
#' recommended by Martín-Andrés and Álvarez-Hernández (2014).
#' Hypothesis testing for diagnostic accuracies uses different methods depending
#' on disease prevalence and number of participants or samples as described by
#' Roldán-Nofuentes and Sidaty-Regad (2019).
#' Global hypothesis testing for predictive values uses a method described by
#' Roldán-Nofuentes et al. (2012), with subsequent individual tests (where
#' indicated) performed using methods described by Kosinksi (2012).
#' The methods for hypothesis testing- for likelihood ratios are taken from
#' Roldán-Nofuentes & Luna del Castillo (2007).
#'
#' An excellent summary of these methods is provided by Roldán-Nofuentes (2020)
#' along with an open-source program (compbdt) licensed under GPL-2.
#' This R package can be considered an extension of this work and is therefore
#' distributed under the same license.
#' Please consider citing Roldán-Nofuentes (2020) when you are citing this
#' package.
#'
#' @references
#' Yu, Guo & Xu (2014) JSCS. 2014; 84:5,1022-1038
#' \doi{10.1080/00949655.2012.738211}
#'
#' Clopper & Pearson (1934) Biometrika. 1934; 26,404-413
#' \doi{10.2307/2331986}
#'
#' Martín Andrés & Álvarez Hernández (2014) Stat Comput. 2014; 24,65–75
#' \doi{10.1007/s11222-012-9353-5}
#'
#' Roldán-Nofuentes & Sidaty-Regad (2019) JSCS. 2019; 89:14,2621-2644
#' \doi{10.1080/00949655.2019.1628234}
#'
#' Roldán-Nofuentes, Luna del Castillo & Montero-Alonso (2012) Comput Stat Data
#' Anal. 2012; 6,1161–1173.
#' \doi{10.1016/j.csda.2011.06.003}
#'
#' Kosinski (2012) Stat Med. 2012; 32,964-977
#' \doi{10.1002/sim.5587}
#'
#' Roldán-Nofuentes, Luna del Castillo (2007) Stat Med. 2007; 26:4179–201.
#' \doi{10.1002/sim.2850}
#'
#' Roldán-Nofuentes (2020) BMC Med Res Methodol. 2020; 20,143
#' \doi{10.1186/s12874-020-00988-y}
#'
#' @examples
#' # load data
#' df <- cfpr
#'
#' # run compareR function
#' compareR(df,
#'   margins = TRUE, multi_corr = "bonf",
#'   test.names = c("pulm.exac", "pseudomonas")
#' )
#'
#' @export
#' @importFrom stats "p.adjust"

compareR <- function(df, alpha = 0.05, margins = FALSE, multi_corr = "holm",
                     cc = TRUE, dp = 1,
                     sesp = TRUE, ppvnpv = TRUE, plrnlr = TRUE,
                     conf.int = "contemporary",
                     test.names = c("Test 1", "Test 2"), ...) {
  df <- recoder(df)

  ## CHECK ARGUMENTS

  if (sesp == FALSE && ppvnpv == FALSE && plrnlr == FALSE) {
    stop("No tests selected.")
  }
  if (ncol(df) != 3) {
    stop("Please provide data as a data frame with three columns.")
  }

  ## FUNCTION

  vals <- values.2test(df)
  cont <- disp.cont(vals, margins = margins)
  prev <- matrixify(conf.prev(vals, alpha = alpha,stats = conf.int), dp = dp+2)

  glob.p.vals <- vector("numeric", 0)
  p.vals <- vector("numeric", 0)

  ## Sens & Spec

  if (sesp == TRUE) {
    acc.est <- conf.acc(vals, alpha = alpha, stats = conf.int)
    acc.inf <- output.acc(vals)

    if (vals$n <= 100 && vals$prev <= 0.1) {
      p.vect <- c(sens = acc.inf$ind.p1, spec = acc.inf$ind.p2)
      t.acc <- c(
        glob = "n < 100 and prevalence <= 10% - global test not used",
        sens = acc.inf$ind.t1, spec = acc.inf$ind.t2
      )
    } else {
      glob.p.vect <- c(acc = acc.inf$glob.p)
      glob.p.vals <- c(glob.p.vals, glob.p.vect)

      if (vals$n <= 100 | vals$n >= 1000) {
        p.vect <- c(sens = acc.inf$ind.p1, spec = acc.inf$ind.p2)
        t.acc <- c(
          glob = acc.inf$glob.t, sens = acc.inf$ind.t1,
          spec = acc.inf$ind.t2
        )
      } else {
        if (cc == TRUE) {
          p.vect <- c(sens = acc.inf$pval3a, spec = acc.inf$pval4a)
          t.acc <- c(
            glob = acc.inf$glob.t,
            sens = acc.inf$Mcc1, spec = acc.inf$Mcc2
          )
        } else {
          p.vect <- c(sens = acc.inf$pval3b, spec = acc.inf$pval4b)
          t.acc <- c(
            glob = acc.inf$glob.t,
            sens = acc.inf$M1, spec = acc.inf$M2
          )
        }
      }
    }
  }

  ## PPV & NPV

  if (ppvnpv == TRUE) {
    pv.est <- conf.pv(vals, alpha = alpha, stats = conf.int)
    pv.inf <- output.pv(vals)

    glob.p.vect <- c(pv = pv.inf$glob.p)
    glob.p.vals <- c(glob.p.vals, glob.p.vect)
    t.pv <- c(glob = pv.inf$glob.t, ppv = pv.inf$ind.t1, npv = pv.inf$ind.t2)
  }

  ## PLR & NLR

  if (plrnlr == TRUE) {
    lr.est <- conf.lr(vals, alpha = alpha)
    lr.inf <- output.lr(vals)

    glob.p.vect <- c(lr = lr.inf$glob.p)
    glob.p.vals <- c(glob.p.vals, glob.p.vect)
    t.lr <- c(glob = lr.inf$glob.t, plr = lr.inf$ind.t1, nlr = lr.inf$ind.t2)
  }

  glob.adj <- p.adjust(glob.p.vals, method = multi_corr, ...)

  for (i in seq_along(glob.adj)) {
    if (is.na(glob.adj[i]) | glob.adj[i] < 0.05) {
      name.vect <- names(glob.adj)[i]

      if ("acc" %in% name.vect) {
        p.vals <- c(p.vals, p.vect)
      }

      if ("pv" %in% name.vect) {
        p.vect <- c(ppv = pv.inf$ind.p1, npv = pv.inf$ind.p2)
        p.vals <- c(p.vals, p.vect)
      }

      if ("lr" %in% name.vect) {
        p.vect <- c(plr = lr.inf$ind.p1, nlr = lr.inf$ind.p2)
        p.vals <- c(p.vals, p.vect)
      }
    }
  }

  p.adj <- p.adjust(p.vals, method = multi_corr, ...)

  if (sesp == TRUE) {
    acc.mat <- matrixify(acc.est,
      rows = c("Sensitivity", "Specificity"),
      test.names = test.names, dp = dp + 2
    )

    acc <- list(
      accuracies = acc.mat,
      glob.test.stat = unname(t.acc["glob"]),
      glob.p.value = unname(glob.p.vals["acc"]),
      glob.p.adj = unname(glob.adj["acc"]),
      sens.test.stat = as.numeric(unname(t.acc["sens"])),
      sens.p.value = unname(p.vals["sens"]),
      sens.p.adj = unname(p.adj["sens"]),
      spec.test.stat = as.numeric(unname(t.acc["spec"])),
      spec.p.value = unname(p.vals["spec"]),
      spec.p.adj = unname(p.adj["spec"])
    )
  }

  if (ppvnpv == TRUE) {
    pv.mat <- matrixify(pv.est,
      rows = c("PPV", "NPV"), test.names = test.names,
      dp = dp + 2
    )

    pv <- list(
      predictive.values = pv.mat,
      glob.test.stat = unname(t.pv["glob"]),
      glob.p.value = unname(glob.p.vals["pv"]),
      glob.p.adj = unname(glob.adj["pv"]),
      ppv.test.stat = unname(t.pv["ppv"]),
      ppv.p.value = unname(p.vals["ppv"]),
      ppv.p.adj = unname(p.adj["ppv"]),
      npv.test.stat = unname(t.pv["npv"]),
      npv.p.value = unname(p.vals["npv"]),
      npv.p.adj = unname(p.adj["npv"])
    )
  }

  if (plrnlr == TRUE) {
    lr.mat <- matrixify(lr.est,
      rows = c("PLR", "NLR"), test.names = test.names,
      dp = dp + 2
    )

    lr <- list(
      likelihood.ratios = lr.mat,
      glob.test.stat = unname(t.lr["glob"]),
      glob.p.value = unname(glob.p.vals["lr"]),
      glob.p.adj = unname(glob.adj["lr"]),
      plr.test.stat = unname(t.lr["plr"]),
      plr.p.value = unname(p.vals["plr"]),
      plr.p.adj = unname(p.adj["plr"]),
      nlr.test.stat = unname(t.lr["nlr"]),
      nlr.p.value = unname(p.vals["nlr"]),
      nlr.p.adj = unname(p.adj["nlr"])
    )
  }

  Y1 <- vals$Se1 + vals$Sp1 - 1
  Y2 <- vals$Se2 + vals$Sp2 - 1

  zeros <- 0
  counts <- c(
    vals$s11, vals$s10, vals$s01, vals$s00,
    vals$r11, vals$r10, vals$r01, vals$r00
  )
  for (i in seq_along(counts)) {
    if (counts[i] == 0) {
      zeros <- zeros + 1
    }
  }

  equal <- FALSE
  if (vals$s10 == vals$s01 && vals$r10 == vals$r01) {
    equal <- TRUE
  }

  other <- list(
    alpha = alpha,
    equal = equal,
    zeros = zeros,
    Youden1 = Y1,
    Youden2 = Y2,
    test.names = test.names
  )

  if (sesp == TRUE && ppvnpv == TRUE && plrnlr == TRUE) {
    out <- list(cont, prev, acc, pv, lr, other)
    names(out) <- c("cont", "prev", "acc", "pv", "lr", "other")
  } else if (sesp == TRUE && ppvnpv == TRUE && plrnlr == FALSE) {
    out <- list(cont, prev, acc, pv, other)
    names(out) <- c("cont", "prev", "acc", "pv", "other")
  } else if (sesp == TRUE && ppvnpv == FALSE && plrnlr == TRUE) {
    out <- list(cont, prev, acc, lr, other)
    names(out) <- c("cont", "prev", "acc", "lr", "other")
  } else if (sesp == FALSE && ppvnpv == TRUE && plrnlr == TRUE) {
    out <- list(cont, prev, pv, lr, other)
    names(out) <- c("cont", "prev", "pv", "lr", "other")
  } else if (sesp == TRUE && ppvnpv == FALSE && plrnlr == FALSE) {
    out <- list(cont, prev, acc, other)
    names(out) <- c("cont", "prev", "acc", "other")
  } else if (sesp == FALSE && ppvnpv == TRUE && plrnlr == FALSE) {
    out <- list(cont, prev, pv, other)
    names(out) <- c("cont", "prev", "pv", "other")
  } else if (sesp == FALSE && ppvnpv == FALSE && plrnlr == TRUE) {
    out <- list(cont, prev, lr, other)
    names(out) <- c("cont", "prev", "lr", "other")
  }

  class(out) <- "compareR"

  return(out)
}

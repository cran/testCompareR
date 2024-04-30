#-------------------------------------------------------------------------------
# RECODING DATA TO AN ACCEPTABLE FORMAT
#-------------------------------------------------------------------------------

#' @title check.df
#'
#' @description Takes a data frame with two or three columns containing the
#' results of binary diagnostic test(s) and a gold standard test and checks it
#' conforms to standards required for the compareR function.
#'
#' @param df A data frame with two or three columns. Users may code positive
#' results as strings (positive, pos, p, yes, y, +, 1), numeric (1) or
#' factor (1). Users may code positive results as strings (negative, neg, no,
#' n, -, 0, 2), numeric (0, 2) or factor (0, 2). Case and white space
#' insensitive.
#'
#' @return A Boolean value indicating whether all entries in the data frame meet
#' the requirements described in arguments.
#'
#' @keywords internal
#'
#' @noRd
#'
check.df <- function(df) {
  # convert matrices to data frame

  if (is.matrix(df)) {
    df <- as.data.frame(df)
  }

  if (!is.data.frame(df)) {
    stop("Data should be provided as a data frame or matrix.")
  }
  if (ncol(df) < 2 | ncol(df) > 3) {
    stop("Data should be provided as a data frame or matrix with two or three
         columns.")
  }

  # define permissible values

  allowed_char <- c(
    "positive", "pos", "p", "yes", "y", "+", "1", "true", "t",
    "negative", "neg", "no", "n", "-", "0", "2", "false", "f"
  )

  allowed_num <- c(
    0, 1, 2
  )

  # convert to lowercase for case-insensitive matching

  df <- as.data.frame(lapply(df, function(col) {
    if (is.character(col)) {
      tolower(trimws(col))
    } else {
      trimws(col)
    }
  }))

  # re-code NA synonyms

  df[df == "na" | df == "not applicable" | df == "n/a"] <- NA

  all_allowed <- all(sapply(df, function(col) {
    all((col %in% allowed_char | col %in% allowed_num) & !is.na(col))
  }))

  if (any(sapply(df, is.na))) {
    stop("Values in data frame not permissable. NAs are not supported.
         See ?check.df for more details.")
  }
  if (all_allowed == FALSE) {
    stop("Values in data frame not permissable. Coding errors exist.
         See ?check.df for more details.")
  }

  return(df)
}

#' @title recoder
#'
#' @description Takes a data frame with two or three columns containing the
#' results of binary diagnostic test(s) and a gold standard test and re-codes
#' to an acceptable format for the compareR function.
#'
#' @param df A dataframe with two or three columns. Users may code positive
#' results as strings (positive, pos, p, yes, y, +, 1), numeric (1) or
#' factor (1). Users may code positive results as strings (negative, neg, no,
#' n, -, 0, 2), numeric (0, 2) or factor (0, 2). Case and white space
#' insensitive.
#'
#' @return A data frame with the same number of columns as df re-coded to
#' integers of 0 (negative) and 1 (positive).
#'
#' @keywords internal
#'
#' @noRd
#'
recoder <- function(df) {
  df <- check.df(df)

  pos <- c("positive", "pos", "p", "yes", "y", "+", "1", "true", "t")
  pos.recode <- 1

  neg <- c("negative", "neg", "no", "n", "-", "0", "2", "false", "f")
  neg.recode <- 0

  df <- as.data.frame(lapply(df, function(col) {
    col <- as.character(col)
    col <- tolower(trimws(col))
    col[col %in% tolower(pos)] <- pos.recode
    col[col %in% tolower(neg)] <- neg.recode
    as.integer(col)
  }))

  return(df)
}

#-------------------------------------------------------------------------------
# COMPUTING VALUES AND CONSTRUCTING CONTINGENCY TABLES
#-------------------------------------------------------------------------------

#' @title values.1test
#'
#' @description Creates a list object containing important values for future
#' calculations.
#'
#' @param df A dataframe with 2 columns (test1, gold), populated with zeros and
#' ones.
#'
#' @return A list object containing important values for future calculations.
#'
#' @keywords internal
#'
#' @noRd
#'
values.1test <- function(df) {
  ## CHECK ARGUMENTS

  if (ncol(df) != 2) {
    stop("Please provide data as a data frame with two columns.")
  }

  ## CALCULATE VALUES

  s1 <- sum(df[, 2] == 1 & df[, 1] == 1)
  s0 <- sum(df[, 2] == 1 & df[, 1] == 0)
  r1 <- sum(df[, 2] == 0 & df[, 1] == 1)
  r0 <- sum(df[, 2] == 0 & df[, 1] == 0)

  ss <- s1 + s0
  rr <- r1 + r0
  n1 <- s1 + r1
  n0 <- s0 + r0
  n <- s1 + s0 + r1 + r0

  p1 <- s1 / n
  p0 <- s0 / n
  q1 <- r1 / n
  q0 <- r0 / n

  prev <- ss / n
  qrev <- 1 - (ss / n)

  Se1 <- s1 / (s1 + s0)
  Sp1 <- r0 / (r0 + r1)

  PPV1 <- (prev * Se1) / (prev * Se1 + qrev * (1 - Sp1))
  NPV1 <- (qrev * Sp1) / (prev * (1 - Se1) + qrev * Sp1)

  PLR1 <- Se1 / (1 - Sp1)
  NLR1 <- (1 - Se1) / Sp1

  VarSe1 <- Se1 * (1 - Se1) / (n * prev)
  VarSp1 <- Sp1 * (1 - Sp1) / (n * qrev)

  VarPPV1 <- s1 * r1 / (n * (s1 + r1)^3)
  VarNPV1 <- s0 * r0 / (n * (s0 + r0)^3)

  ## VAR of PPV/NPV - ALSO SEEMS WRONG IN 2test function

  VarPLR1 <- (Se1^2 * VarSp1 + (1 - Sp1)^2 * VarSe1) / (1 - Sp1)^4
  VarNLR1 <- ((1 - Se1)^2 * VarSp1 + Sp1^2 * VarSe1) / Sp1^4

  vals <- list(
    s1 = s1, s0 = s0, r1 = r1, r0 = r0,
    ss = ss, rr = rr, n1 = n1, n0 = n0, n = n,
    p1 = p1, p0 = p0, q1 = q1, q0 = q0,
    prev = prev, qrev = qrev,
    Se1 = Se1, Sp1 = Sp1,
    PPV1 = PPV1, NPV1 = NPV1,
    PLR1 = PLR1, NLR1 = NLR1,
    VarSe1 = VarSe1, VarSp1 = VarSp1,
    VarPPV1 = VarPPV1, VarNPV1 = VarNPV1,
    VarPLR1 = VarPLR1, VarNLR1 = VarNLR1
  )

  class(vals) <- "vals.1test"
  return(vals)
}

#' @title values.2test
#'
#' @description Creates a list object containing important values for future
#' calculations.
#'
#' @param df A dataframe with 3 columns (test1, test2, gold), populated with
#' zeros and ones.
#'
#' @return A list object containing important values for future calculations.
#'
#' @keywords internal
#'
#' @noRd
#'
values.2test <- function(df) {
  ## CHECK ARGUMENTS

  if (ncol(df) != 3) {
    stop("Please provide data as a data frame with three columns.")
  }

  ## CALCULATE VALUES

  s11 <- sum(df[, 3] == 1 & df[, 1] == 1 & df[, 2] == 1)
  s10 <- sum(df[, 3] == 1 & df[, 1] == 1 & df[, 2] == 0)
  s01 <- sum(df[, 3] == 1 & df[, 1] == 0 & df[, 2] == 1)
  s00 <- sum(df[, 3] == 1 & df[, 1] == 0 & df[, 2] == 0)
  r11 <- sum(df[, 3] == 0 & df[, 1] == 1 & df[, 2] == 1)
  r10 <- sum(df[, 3] == 0 & df[, 1] == 1 & df[, 2] == 0)
  r01 <- sum(df[, 3] == 0 & df[, 1] == 0 & df[, 2] == 1)
  r00 <- sum(df[, 3] == 0 & df[, 1] == 0 & df[, 2] == 0)

  ss <- sum(s11, s10, s01, s00)
  rr <- sum(r11, r10, r01, r00)
  n11 <- s11 + r11
  n10 <- s10 + r10
  n01 <- s01 + r01
  n00 <- s00 + r00
  n <- sum(s11, s10, s01, s00, r11, r10, r01, r00)

  p11 <- s11 / n
  p10 <- s10 / n
  p01 <- s01 / n
  p00 <- s00 / n
  q11 <- r11 / n
  q10 <- r10 / n
  q01 <- r01 / n
  q00 <- r00 / n

  prev <- ss / n
  qrev <- 1 - (ss / n)

  Se1 <- (s11 + s10) / ss
  Se2 <- (s11 + s01) / ss
  Sp1 <- (r00 + r01) / rr
  Sp2 <- (r00 + r10) / rr

  PPV1 <- (prev * Se1) / (prev * Se1 + qrev * (1 - Sp1))
  PPV2 <- (prev * Se2) / (prev * Se2 + qrev * (1 - Sp2))
  NPV1 <- (qrev * Sp1) / (prev * (1 - Se1) + qrev * Sp1)
  NPV2 <- (qrev * Sp2) / (prev * (1 - Se2) + qrev * Sp2)

  PLR1 <- Se1 / (1 - Sp1)
  PLR2 <- Se2 / (1 - Sp2)
  NLR1 <- (1 - Se1) / Sp1
  NLR2 <- (1 - Se2) / Sp2

  VarSe1 <- Se1 * (1 - Se1) / (n * prev)
  VarSe2 <- Se2 * (1 - Se2) / (n * prev)
  VarSp1 <- Sp1 * (1 - Sp1) / (n * qrev)
  VarSp2 <- Sp2 * (1 - Sp2) / (n * qrev)

  VarPPV1 <- ((p10 + p11) * (q10 + q11)) / (n * (p10 + p11 + q10 + q11)^3)
  VarPPV2 <- ((p01 + p11) * (q01 + q11)) / (n * (p01 + p11 + q01 + q11)^3)

  VarNPV1 <- ((p00 + p01) * (q00 + q01)) / (n * (p00 + p01 + q00 + q01)^3)
  VarNPV2 <- ((p00 + p10) * (q00 + q10)) / (n * (p00 + p10 + q00 + q10)^3)

  VarPLR1 <- (Se1^2 * VarSp1 + (1 - Sp1)^2 * VarSe1) / (1 - Sp1)^4
  VarPLR2 <- (Se2^2 * VarSp2 + (1 - Sp2)^2 * VarSe2) / (1 - Sp2)^4

  VarNLR1 <- ((1 - Se1)^2 * VarSp1 + Sp1^2 * VarSe1) / Sp1^4
  VarNLR2 <- ((1 - Se2)^2 * VarSp2 + Sp2^2 * VarSe2) / Sp2^4

  vals <- list(
    s11 = s11, s10 = s10, s01 = s01, s00 = s00,
    r11 = r11, r10 = r10, r01 = r01, r00 = r00,
    ss = ss, rr = rr,
    n11 = n11, n10 = n10, n01 = n01, n00 = n00, n = n,
    p11 = p11, p10 = p10, p01 = p01, p00 = p00,
    q11 = q11, q10 = q10, q01 = q01, q00 = q00,
    prev = prev, qrev = qrev,
    Se1 = Se1, Se2 = Se2, Sp1 = Sp1, Sp2 = Sp2,
    PPV1 = PPV1, PPV2 = PPV2, NPV1 = NPV1, NPV2 = NPV2,
    PLR1 = PLR1, PLR2 = PLR2, NLR1 = NLR1, NLR2 = NLR2,
    VarSe1 = VarSe1, VarSe2 = VarSe2,
    VarSp1 = VarSp1, VarSp2 = VarSp2,
    VarPPV1 = VarPPV1, VarPPV2 = VarPPV2,
    VarNPV1 = VarNPV1, VarNPV2 = VarNPV2,
    VarPLR1 = VarPLR1, VarPLR2 = VarPLR2,
    VarNLR1 = VarNLR1, VarNLR2 = VarNLR2
  )

  class(vals) <- "vals.2test"
  return(vals)
}

#' @title disp.cont
#'
#' @description Creates and displays a contingency table of positive and
#' negative test results and disease status.
#'
#' @param vals A list object containing all values required for calculations
#' with class `'vals.1test'` or `'vals.2test'`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param margins A Boolean value indicating whether the outputted matrix should
#' have margins containing summed totals of rows and columns.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return Either a matrix or list of matrices displaying counts of positive and
#' negative tests by true disease status.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "addmargins"
#'
disp.cont <- function(vals, margins = FALSE, ...) {
  ## CHECK ARGUMENTS

  if (!(class(vals) %in% c("vals.1test", "vals.2test"))) {
    stop("Argument must be of \"vals.1test\" or \"vals.2test\" class.")
  }

  ## COMPUTE CONTINGENCY TABLE

  if (inherits(vals, "vals.1test")) {
    mat <- matrix(c(vals$s1, vals$r1, vals$s0, vals$r0), nrow = 2)

    dimnames(mat) <- list(
      "True Status" = c("Positive", "Negative"),
      "Test" = c("Positive", "Negative")
    )

    if (margins == TRUE) {
      mat <- addmargins(mat, ...)
    }

    return(mat)
  } else if (inherits(vals, "vals.2test")) {
    mat1 <- matrix(c(vals$s11, vals$s01, vals$s10, vals$s00), nrow = 2)
    dimnames(mat1) <- list(
      "Test 1" = c("Positive", "Negative"),
      "Test 2" = c("Positive", "Negative")
    )

    mat2 <- matrix(c(vals$r11, vals$r01, vals$r10, vals$r00), nrow = 2)
    dimnames(mat2) <- list(
      "Test 1" = c("Positive", "Negative"),
      "Test 2" = c("Positive", "Negative")
    )

    mat3 <- matrix(c(vals$s11 + vals$s10,
                     vals$r11 + vals$r10,
                     vals$s01 + vals$s00,
                     vals$r01 + vals$r00),
                   nrow = 2)
    dimnames(mat3) <- list(
      "Gold standard" = c("Positive", "Negative"),
      "Test 1" = c("Positive", "Negative")
    )

    mat4 <- matrix(c(vals$s11 + vals$s01,
                     vals$r11 + vals$r01,
                     vals$s10 + vals$s00,
                     vals$r10 + vals$r00),
                   nrow = 2)
    dimnames(mat4) <- list(
      "Gold standard" = c("Positive", "Negative"),
      "Test 2" = c("Positive", "Negative")
    )

    if (margins == TRUE) {
      mat1 <- addmargins(mat1, ...)
      mat2 <- addmargins(mat2, ...)
      mat3 <- addmargins(mat3, ...)
      mat4 <- addmargins(mat4, ...)
    }

    mat <- list(mat3, mat4, mat1, mat2)
    names(mat) <- c("Gold standard vs. Test 1", "Gold standard vs. Test 2",
                    "True Status: POS", "True Status: NEG")

    return(mat)
  }
}

#-------------------------------------------------------------------------------
# CALCULATING CONFIDENCE INTERVALS
#-------------------------------------------------------------------------------

#' @title yu.int
#'
#' @description Calculates the Yu et al. confidence interval for binomial
#' proportions.
#'
#' @param n Total number of cases evaluated.
#' @param z Z-score of normal distribution at a defined alpha level.
#' @param est The point estimate of the specific test under evaluation.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object containing lower and upper confidence bounds.
#'
#' @keywords internal
#'
#' @noRd
#'
yu.int <- function(n, z, est) {
  # CHECK ARGUMENTS

  if (n < 0) stop("n must be a non-negative integer.")
  if (!is.integer(n)) stop("n must be a non-negative integer.")
  if (z < 0) stop("z must not be negative.")
  if (!is.na(est) && (est < 0 | est > 1)) {
    stop("Point estimate must be between 0 and 1, inclusive.")
  }

  # FUNCTION

  lower <- 0.5 + ((n + z^4 / 53) / (n + z^2)) * (est - 0.5) - (z / (n + z^2)) *
    sqrt(n * est * (1 - est) + z^2 / 4)
  upper <- 0.5 + ((n + z^4 / 53) / (n + z^2)) * (est - 0.5) + (z / (n + z^2)) *
    sqrt(n * est * (1 - est) + z^2 / 4)

  if (is.na(est)) {
    return(list(lower = 0, upper = 1))
  } else if (lower >= 0 && upper <= 1) {
    return(list(lower = lower, upper = upper))
  } else if (lower >= 0 && upper > 1) {
    return(list(lower = lower, upper = 1))
  } else if (lower < 0 && upper <= 1) {
    return(list(lower = 0, upper = upper))
  } else {
    stop("Confidence interval includes values less than zero and greater than
         one. Check input and consider reporting a bug.")
  }
}

#' @title conf.prev
#'
#' @description Calculates confidence intervals for disease prevalence.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param alpha An alpha value. Defaults to 0.05.
#' @param stats A character string, either "contemporary" or "classic".
#' Indicates whether function should use contemporary or classic statistical
#' methods.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object of class `'prev'` containing a point estimate, plus
#' lower and upper confidence bounds for disease prevalence.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "qnorm" "binom.test"

conf.prev <- function(vals, alpha = 0.05, stats = "contemporary", ...) {
  ## CHECK ARGUMENTS

  if (!(class(vals) %in% c("vals.1test", "vals.2test"))) {
    stop("Argument must be of \"vals.1test\" or \"vals.2test\" class.")
  }
  if (alpha < 0 | alpha > 1) {
    stop("Alpha must be a value between 0 and 1, inclusive.")
  }
  if (is.na(vals$prev)) {
    stop("Prevalence should not be NA/NaN. Check data and consider reporting
         a bug.")
  }

  ## CALCULATE CONFIDENCE INTERVALS

  conf <- 1 - alpha
  z <- qnorm(1 - alpha / 2, ...)

  Varprev <- vals$prev * vals$qrev / vals$n

  if (stats == "contemporary") {
    yu <- yu.int(vals$n, z, vals$prev)

    ci.prev <- list(
      est = vals$prev,
      SE = sqrt(Varprev),
      ci.lower = yu$lower,
      ci.upper = yu$upper
    )
  } else if (stats == "classic") {
    cp <- binom.test(c(vals$ss, vals$rr), vals$prev)$conf.int

    ci.prev <- list(
      est = vals$prev,
      SE = sqrt(Varprev),
      ci.lower = cp[1],
      ci.upper = cp[2]
    )
  } else {
    stop("stats argument must be either \"contemporary\" or \"classic\"")
  }


  class(ci.prev) <- "prev"
  return(ci.prev)
}

#' @title conf.acc
#'
#' @description Calculates confidence intervals for accuracies (sensitivity and
#' specificity) for one or two binary diagnostic tests.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param alpha An alpha value. Defaults to 0.05.
#' @param stats A character string, either "contemporary" or "classic".
#' Indicates whether function should use contemporary or classic statistical
#' methods.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object of class `'conf.1t'` or `'conf.2t'` containing point
#' estimates, plus lower and upper confidence bounds for diagnostic accuracies.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "qnorm" "binom.test"

conf.acc <- function(vals, alpha = 0.05, stats = "contemporary", ...) {
  ## CHECK ARGUMENTS

  if (!(class(vals) %in% c("vals.1test", "vals.2test"))) {
    stop("Argument must be of \"vals.1test\" or \"vals.2test\" class.")
  }
  if (alpha < 0 | alpha > 1) {
    stop("Alpha must be a value between 0 and 1, inclusive.")
  }

  ## CALCULATE CONFIDENCE INTERVALS

  conf <- 1 - alpha
  z <- qnorm(1 - alpha / 2, ...)

  if (inherits(vals, "vals.2test")) {

    if (stats == "contemporary") {
      yu_Se1 <- yu.int(vals$ss, z, vals$Se1)
      yu_Se2 <- yu.int(vals$ss, z, vals$Se2)
      yu_Sp1 <- yu.int(vals$rr, z, vals$Sp1)
      yu_Sp2 <- yu.int(vals$rr, z, vals$Sp2)

      ci.acc <- list(
        test1.se = list(
          est = vals$Se1,
          SE = sqrt(vals$VarSe1),
          ci.lower = yu_Se1$lower,
          ci.upper = yu_Se1$upper
        ),
        test2.se = list(
          est = vals$Se2,
          SE = sqrt(vals$VarSe2),
          ci.lower = yu_Se2$lower,
          ci.upper = yu_Se2$upper
        ),
        test1.sp = list(
          est = vals$Sp1,
          SE = sqrt(vals$VarSp1),
          ci.lower = yu_Sp1$lower,
          ci.upper = yu_Sp1$upper
        ),
        test2.sp = list(
          est = vals$Sp2,
          SE = sqrt(vals$VarSp2),
          ci.lower = yu_Sp2$lower,
          ci.upper = yu_Sp2$upper
        )
      )
    } else if (stats == "classic") {
      cp_Se1 <- binom.test(c(vals$s11 + vals$s10,
                             vals$s01 + vals$s00), vals$Se1)$conf.int
      cp_Se2 <- binom.test(c(vals$s11 + vals$s01,
                             vals$s10 + vals$s00), vals$Se2)$conf.int
      cp_Sp1 <- binom.test(c(vals$r00 + vals$r01,
                             vals$r10 + vals$r11), vals$Sp1)$conf.int
      cp_Sp2 <- binom.test(c(vals$r00 + vals$r10,
                             vals$r01 + vals$r11), vals$Sp2)$conf.int

      ci.acc <- list(
        test1.se = list(
          est = vals$Se1,
          SE = sqrt(vals$VarSe1),
          ci.lower = cp_Se1[1],
          ci.upper = cp_Se1[2]
        ),
        test2.se = list(
          est = vals$Se2,
          SE = sqrt(vals$VarSe2),
          ci.lower = cp_Se2[1],
          ci.upper = cp_Se2[2]
        ),
        test1.sp = list(
          est = vals$Sp1,
          SE = sqrt(vals$VarSp1),
          ci.lower = cp_Sp1[1],
          ci.upper = cp_Sp1[2]
        ),
        test2.sp = list(
          est = vals$Sp2,
          SE = sqrt(vals$VarSp2),
          ci.lower = cp_Sp2[1],
          ci.upper = cp_Sp2[2]
        )
      )
    } else {
      stop("stats argument must be either \"contemporary\" or \"classic\"")
    }

    class(ci.acc) <- "conf.2t"

    return(ci.acc)

  } else if (inherits(vals, "vals.1test")) {

    if (stats == "contemporary") {
      yu_Se1 <- yu.int(vals$ss, z, vals$Se1) # bug in original? didn't give same answer as above
      yu_Sp1 <- yu.int(vals$rr, z, vals$Sp1) # bug in original? didn't give same answer as above

      ci.acc <- list(
        se = list(
          est = vals$Se1,
          SE = sqrt(vals$VarSe1),
          ci.lower = yu_Se1$lower,
          ci.upper = yu_Se1$upper
        ),
        sp = list(
          est = vals$Sp1,
          SE = sqrt(vals$VarSp1),
          ci.lower = yu_Sp1$lower,
          ci.upper = yu_Sp1$upper
        )
      )
    } else if (stats == "classic") {
      cp_Se1 <- binom.test(c(vals$s1, vals$s0), vals$Se1)$conf.int
      cp_Sp1 <- binom.test(c(vals$r0, vals$r1), vals$Se1)$conf.int

      ci.acc <- list(
        se = list(
          est = vals$Se1,
          SE = sqrt(vals$VarSe1),
          ci.lower = cp_Se1[1],
          ci.upper = cp_Se1[2]
        ),
        sp = list(
          est = vals$Sp1,
          SE = sqrt(vals$VarSp1),
          ci.lower = cp_Sp1[1],
          ci.upper = cp_Sp1[2]
        )
      )
    } else {
      stop("stats argument must be either \"contemporary\" or \"classic\"")
    }

    class(ci.acc) <- "conf.1t"

    return(ci.acc)
  }
}

#' @title conf.pv
#'
#' @description Calculates confidence intervals for positive and negative
#' predictive values for one or two binary diagnostic tests.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param alpha An alpha value. Defaults to 0.05.
#' @param stats A character string, either "contemporary" or "classic".
#' Indicates whether function should use contemporary or classic statistical
#' methods.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object of class `'conf.1t'` or `'conf.2t'` containing point
#' estimates, plus lower and upper confidence bounds for predictive values.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "qnorm" "binom.test"

conf.pv <- function(vals, alpha = 0.05, stats = "contemporary", ...) {
  ## CHECK ARGUMENTS

  if (!(class(vals) %in% c("vals.1test", "vals.2test"))) {
    stop("Argument must be of \"vals.1test\" or \"vals.2test\" class.")
  }
  if (alpha < 0 | alpha > 1) {
    stop("Alpha must be a value between 0 and 1, inclusive.")
  }

  ## CALCULATE CONFIDENCE INTERVALS

  conf <- 1 - alpha
  z <- qnorm(1 - alpha / 2, ...)

  if (inherits(vals, "vals.2test")) {

    if (stats == "contemporary") {
      yu_PPV1 <- yu.int(vals$n11 + vals$n10, z, vals$PPV1)
      yu_PPV2 <- yu.int(vals$n11 + vals$n01, z, vals$PPV2)
      yu_NPV1 <- yu.int(vals$n01 + vals$n00, z, vals$NPV1)
      yu_NPV2 <- yu.int(vals$n10 + vals$n00, z, vals$NPV2)

      ci.pv <- list(
        test1.ppv = list(
          est = vals$PPV1,
          SE = sqrt(vals$VarPPV1),
          ci.lower = yu_PPV1$lower,
          ci.upper = yu_PPV1$upper
        ),
        test2.ppv = list(
          est = vals$PPV2,
          SE = sqrt(vals$VarPPV2),
          ci.lower = yu_PPV2$lower,
          ci.upper = yu_PPV2$upper
        ),
        test1.npv = list(
          est = vals$NPV1,
          SE = sqrt(vals$VarNPV1),
          ci.lower = yu_NPV1$lower,
          ci.upper = yu_NPV1$upper
        ),
        test2.npv = list(
          est = vals$NPV2,
          SE = sqrt(vals$VarNPV2),
          ci.lower = yu_NPV2$lower,
          ci.upper = yu_NPV2$upper
        )
      )
    } else if (stats == "classic") {
      cp_PPV1 <- binom.test(c(vals$s11 + vals$s10,
                              vals$r10 + vals$r11), vals$PPV1)$conf.int
      cp_PPV2 <- binom.test(c(vals$s11 + vals$s01,
                              vals$r01 + vals$r11), vals$PPV2)$conf.int
      cp_NPV1 <- binom.test(c(vals$r00 + vals$r01,
                              vals$s01 + vals$s00), vals$NPV1)$conf.int
      cp_NPV2 <- binom.test(c(vals$r00 + vals$r10,
                              vals$s10 + vals$s00), vals$NPV2)$conf.int

      ci.pv <- list(
        test1.ppv = list(
          est = vals$PPV1,
          SE = sqrt(vals$VarPPV1),
          ci.lower = cp_PPV1[1],
          ci.upper = cp_PPV1[2]
        ),
        test2.ppv = list(
          est = vals$PPV2,
          SE = sqrt(vals$VarPPV2),
          ci.lower = cp_PPV2[1],
          ci.upper = cp_PPV2[2]
        ),
        test1.npv = list(
          est = vals$NPV1,
          SE = sqrt(vals$VarNPV1),
          ci.lower = cp_NPV1[1],
          ci.upper = cp_NPV1[2]
        ),
        test2.npv = list(
          est = vals$NPV2,
          SE = sqrt(vals$VarNPV2),
          ci.lower = cp_NPV2[1],
          ci.upper = cp_NPV2[2]
        )
      )
    } else {
      stop("stats argument must be either \"contemporary\" or \"classic\"")
    }

    class(ci.pv) <- "conf.2t"

    return(ci.pv)
  } else if (inherits(vals, "vals.1test")) {

    if (stats == "contemporary") {
      yu_PPV1 <- yu.int(vals$n, z, vals$PPV1)
      yu_NPV1 <- yu.int(vals$n, z, vals$NPV1)

      ci.pv <- list(
        PPV = list(
          est = vals$PPV1,
          SE = sqrt(vals$VarPPV1),
          ci.lower = yu_PPV1$lower,
          ci.upper = yu_PPV1$upper
        ),
        NPV = list(
          est = vals$NPV1,
          SE = sqrt(vals$VarNPV1),
          ci.lower = yu_NPV1$lower,
          ci.upper = yu_NPV1$upper
        )
      )
    } else if (stats == "classic") {
      cp_PPV1 <- binom.test(c(vals$s1, vals$r1), vals$PPV1)$conf.int
      cp_NPV1 <- binom.test(c(vals$r0, vals$s0), vals$NPV1)$conf.int

      ci.pv <- list(
        PPV = list(
          est = vals$PPV1,
          SE = sqrt(vals$VarPPV1),
          ci.lower = cp_PPV1[1],
          ci.upper = cp_PPV1[2]
        ),
        NPV = list(
          est = vals$NPV1,
          SE = sqrt(vals$VarNPV1),
          ci.lower = cp_NPV1[1],
          ci.upper = cp_NPV1[2]
        )
      )
    } else {
      stop("stats argument must be either \"contemporary\" or \"classic\"")
    }

    class(ci.pv) <- "conf.1t"

    return(ci.pv)
  }
}

#' @title ciplr
#'
#' @description Calculates confidence intervals for positive likelihood ratio.
#'
#' @param s1 Number of cases with a positive test and positive disease status.
#' @param s0 Number of cases with a positive test and negative disease status.
#' @param r1 Number of cases with a negative test and positive disease status.
#' @param r0 Number of cases with a negative test and negative disease status.
#' @param z Z-score of normal distribution at a defined alpha level.
#'
#' @return A list object of class `'conf.1t'` or `'conf.2t'` containing lower
#' and upper confidence bounds for positive likelihood ratio.
#'
#' @keywords internal
#'
#' @noRd
#'
ciplr <- function(s1, s0, r1, r0, z) {
  ss1 <- s1 + s0
  rr1 <- r1 + r0
  nn1 <- ss1 + rr1

  p1 <- (r1 + 0.5) / (rr1 + 1)
  p2 <- (s1 + 0.5) / (ss1 + 1)

  Lplr <- ((nn1 + 2) * (s1 + 0.5) * (r1 + 0.5) + (z^2 / 2) * ((ss1 + 1) *
    (s1 + 0.5) + (rr1 + 1) * (r1 + 0.5) - 2 * (s1 + 0.5) * (r1 + 0.5)) -
    z * sqrt(((nn1 + 2)^2 * (r1 + 0.5) * (s1 + 0.5) * ((s1 + r1 + 1) -
      (nn1 + 2) * p1 * p2) + (z^2 / 4) * ((ss1 + 1) * (s1 + 0.5) - (rr1 + 1)
    * (r1 + 0.5))^2))) / ((r1 + 0.5) * ((nn1 + 2) * (ss1 + 1) * p1 - z^2
    * ((ss1 + 1) - (r1 + 0.5))))

  Uplr <- ((nn1 + 2) * (r1 + 0.5) * (s1 + 0.5) + (z^2 / 2) * ((ss1 + 1) *
    (s1 + 0.5) + (rr1 + 1) * (r1 + 0.5) - 2 * (r1 + 0.5) * (s1 + 0.5)) +
    z * sqrt(((nn1 + 2)^2 * (r1 + 0.5) * (s1 + 0.5) * ((s1 + r1 + 1) -
      (nn1 + 2) * p1 * p2) + (z^2 / 4) * ((ss1 + 1) * (s1 + 0.5) - (rr1 + 1)
    * (r1 + 0.5))^2))) / ((r1 + 0.5) * ((nn1 + 2) * (ss1 + 1) * p1 - z^2
    * ((ss1 + 1) - (r1 + 0.5))))

  if (Lplr < (s1 + 0.5) / ((nn1 + 2) - (r1 + 0.5))) {
    Lplr <- ((s1 + 0.5) * p1 + z^2 / 2 - z * sqrt(z^2 / 4 + (s1 + 0.5) *
      (p1 - p2))) / ((ss1 + 1) * p1^2 + z^2)
  }
  if (Uplr > ((nn1 + 2) - (s1 + 0.5)) / (r1 + 0.5)) {
    Uplr <- ((r1 + 0.5) * p2 + z^2 / 2 + z * sqrt(z^2 / 4 + (r1 + 0.5) *
      (p2 - p1))) / ((rr1 + 1) * p1^2)
  }

  ciPLR <- list(LPLR = Lplr, UPLR = Uplr)
}

#' @title cinlr
#'
#' @description Calculates confidence intervals for negative likelihood ratio.
#'
#' @param s1 Number of cases with a positive test and positive disease status.
#' @param s0 Number of cases with a positive test and negative disease status.
#' @param r1 Number of cases with a negative test and positive disease status.
#' @param r0 Number of cases with a negative test and negative disease status.
#' @param z Z-score of normal distribution at a defined alpha level.
#'
#' @return A list object of class `'conf.1t'` or `'conf.2t'` containing lower
#' and upper confidence bounds for negative likelihood ratio.
#'
#' @keywords internal
#'
#' @noRd
#'
cinlr <- function(s1, s0, r1, r0, z) {
  ss1 <- s1 + s0
  rr1 <- r1 + r0
  nn1 <- ss1 + rr1

  p1 <- (r0 + 0.5) / (rr1 + 1)
  p2 <- (s0 + 0.5) / (ss1 + 1)

  Lnlr <- ((nn1 + 2) * (r0 + 0.5) * (s0 + 0.5) + (z^2 / 2) * ((ss1 + 1) *
    (s0 + 0.5) + (rr1 + 1) * (r0 + 0.5) - 2 * (r0 + 0.5) * (s0 + 0.5)) -
    z * sqrt(((nn1 + 2)^2 * (r0 + 0.5) * (s0 + 0.5) * ((s0 + r0 + 1) -
      (nn1 + 2) * p1 * p2) + (z^2 / 4) * ((ss1 + 1) * (s0 + 0.5) - (rr1 + 1)
    * (r0 + 0.5))^2))) / ((r0 + 0.5) * ((nn1 + 2) * (ss1 + 1) * p1 - z^2
    * ((ss1 + 1) - (r0 + 0.5))))

  Unlr <- ((nn1 + 2) * (r0 + 0.5) * (s0 + 0.5) + (z^2 / 2) * ((ss1 + 1) *
    (s0 + 0.5) + (rr1 + 1) * (r0 + 0.5) - 2 * (r0 + 0.5) * (s0 + 0.5)) +
    z * sqrt(((nn1 + 2)^2 * (r0 + 0.5) * (s0 + 0.5) * ((s0 + r0 + 1) -
      (nn1 + 2) * p1 * p2) + (z^2 / 4) * ((ss1 + 1) * (s0 + 0.5) - (rr1 + 1)
    * (r0 + 0.5))^2))) / ((r0 + 0.5) * ((nn1 + 2) * (ss1 + 1) * p1 - z^2
    * ((ss1 + 1) - (r0 + 0.5))))

  if (Lnlr < (s0 + 0.5) / ((nn1 + 2) - (r0 + 0.5))) {
    Lnlr <- ((s0 + 0.5) * p1 + z^2 / 2 - z * sqrt(z^2 / 4 + (s0 + 0.5) *
      (p1 - p2))) / ((ss1 + 1) * p1^2 + z^2)
  }
  if (Unlr > ((nn1 + 2) - (s0 + 0.5)) / (r0 + 0.5)) {
    Unlr <- ((r0 + 0.5) * p2 + z^2 / 2 + z * sqrt(z^2 / 4 + (r0 + 0.5) *
      (p2 - p1))) / ((rr1 + 1) * p1^2)
  }

  ciNLR <- list(LNLR = Lnlr, UNLR = Unlr)
}

#' @title conf.lr
#'
#' @description Calculates confidence intervals for positive and negative
#' likelihood ratios for one or two binary diagnostic tests.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param alpha An alpha value. Defaults to 0.05.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object of class `'conf.1t'` or `'conf.2t'` containing point
#' estimates, plus lower and upper confidence bounds for likelihood ratios.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "qnorm"

conf.lr <- function(vals, alpha = 0.05, ...) {
  ## CHECK ARGUMENTS

  if (!(class(vals) %in% c("vals.1test", "vals.2test"))) {
    stop("Argument must be of \"vals.1test\" or \"vals.2test\" class.")
  }
  if (alpha < 0 | alpha > 1) {
    stop("Alpha must be a value between 0 and 1, inclusive.")
  }

  ## CALCULATE CONFIDENCE INTERVALS

  conf <- 1 - alpha
  z <- qnorm(1 - alpha / 2, ...)

  if (inherits(vals, "vals.2test")) {
    ciPLR1 <- ciplr(
      vals$s11 + vals$s10, vals$s01 + vals$s00,
      vals$r11 + vals$r10, vals$r01 + vals$r00, z
    )
    ciPLR2 <- ciplr(
      vals$s11 + vals$s01, vals$s10 + vals$s00,
      vals$r11 + vals$r01, vals$r10 + vals$r00, z
    )

    ciNLR1 <- cinlr(
      vals$s11 + vals$s10, vals$s01 + vals$s00,
      vals$r11 + vals$r10, vals$r01 + vals$r00, z
    )
    ciNLR2 <- cinlr(
      vals$s11 + vals$s01, vals$s10 + vals$s00,
      vals$r11 + vals$r01, vals$r10 + vals$r00, z
    )

    ci.lr <- list(
      test1.plr = list(
        est = vals$PLR1,
        SE = sqrt(vals$VarPLR1),
        ci.lower = ciPLR1$LPLR,
        ci.upper = ciPLR1$UPLR
      ),
      test2.plr = list(
        est = vals$PLR2,
        SE = sqrt(vals$VarPLR2),
        ci.lower = ciPLR2$LPLR,
        ci.upper = ciPLR2$UPLR
      ),
      test1.nlr = list(
        est = vals$NLR1,
        SE = sqrt(vals$VarNLR1),
        ci.lower = ciNLR1$LNLR,
        ci.upper = ciNLR1$UNLR
      ),
      test2.nlr = list(
        est = vals$NLR2,
        SE = sqrt(vals$VarNLR2),
        ci.lower = ciNLR2$LNLR,
        ci.upper = ciNLR2$UNLR
      )
    )

    class(ci.lr) <- "conf.2t.lr"

    return(ci.lr)
  } else if (inherits(vals, "vals.1test")) {
    ciPLR1 <- ciplr(
      vals$s1, vals$s0,
      vals$r1, vals$r0, z
    )

    ciNLR1 <- cinlr(
      vals$s1, vals$s0,
      vals$r1, vals$r0, z
    )

    ci.lr <- list(
      PLR = list(
        est = vals$PLR1,
        SE = sqrt(vals$VarPLR1),
        ci.lower = ciPLR1$LPLR,
        ci.upper = ciPLR1$UPLR
      ),
      NLR = list(
        est = vals$NLR1,
        SE = sqrt(vals$VarNLR1),
        ci.lower = ciNLR1$LNLR,
        ci.upper = ciNLR1$UNLR
      )
    )

    class(ci.lr) <- "conf.1t.lr"

    return(ci.lr)
  }
}

#-------------------------------------------------------------------------------
# PERFORMING STATISTICAL INFERENCE
#-------------------------------------------------------------------------------

#' @title output.acc
#'
#' @description Performs statistical inference tests on the diagnostic
#' accuracies of two binary diagnostic tests.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object containing test statistics and p-values for global and
#' individual hypothesis tests of diagnostic accuracies.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "pchisq"

output.acc <- function(vals, ...) {
  ## CHECK ARGUMENTS

  if (!inherits(vals, "vals.2test")) {
    stop("Argument must be of \"vals.2test\" class.")
  }

  ## GLOBAL HYPOTHESIS TEST

  Q1 <- vals$ss * (vals$s10 - vals$s01)^2 / (4 * vals$s10 * vals$s01 +
    (vals$s11 + vals$s00) * (vals$s10 + vals$s01)) + vals$rr *
    (vals$r10 - vals$r01)^2 / (4 * vals$r10 * vals$r01 +
      (vals$r11 + vals$r00) * (vals$r10 + vals$r01))

  globalpvalue1 <- (1 - pchisq(Q1, 2, ...))

  ## INDIVIDUAL HYPOTHESIS TESTS
  ## WALD TEST STATISTIC AND P-VALUES

  w1 <- (vals$ss * (vals$s10 - vals$s01)^2) / (4 * vals$s10 * vals$s01 +
    (vals$s11 + vals$s00) * (vals$s10 + vals$s01))
  w2 <- (vals$rr * (vals$r10 - vals$r01)^2) / (4 * vals$r10 * vals$r01 +
    (vals$r11 + vals$r00) * (vals$r10 + vals$r01))

  pvalue1 <- 1 - pchisq(w1, 1, ...)
  pvalue2 <- 1 - pchisq(w2, 1, ...)

  ## MCNEMARS CHI-SQUARED AND P-VALUES

  Mcc1 <- (abs(vals$s10 - vals$s01) - 1)^2 / (vals$s10 + vals$s01)
  Mcc2 <- (abs(vals$r10 - vals$r01) - 1)^2 / (vals$r10 + vals$r01)

  pvalue3a <- 1 - pchisq(Mcc1, 1, ...)
  pvalue4a <- 1 - pchisq(Mcc2, 1, ...)

  M1 <- abs(vals$s10 - vals$s01)^2 / (vals$s10 + vals$s01)
  M2 <- abs(vals$r10 - vals$r01)^2 / (vals$r10 + vals$r01)

  pvalue3b <- 1 - pchisq(M1, 1, ...)
  pvalue4b <- 1 - pchisq(M2, 1, ...)

  return(list(
    glob.t = Q1, glob.p = globalpvalue1,
    ind.t1 = w1, ind.t2 = w2,
    ind.p1 = pvalue1, ind.p2 = pvalue2,
    Mcc1 = Mcc1, Mcc2 = Mcc2,
    pval3a = pvalue3a, pval4a = pvalue4a,
    M1 = M1, M2 = M2,
    pval3b = pvalue3b, pval4b = pvalue4b
  ))
}

#' @title output.pv
#'
#' @description Performs statistical inference tests on the predictive values
#' of two binary diagnostic tests.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object containing test statistics and p-values for global and
#' individual hypothesis tests of predictive values.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "pchisq"

output.pv <- function(vals, ...) {
  ## CHECK ARGUMENTS

  if (!inherits(vals, "vals.2test")) {
    stop("Argument must be of \"vals.2test\" class.")
  }

  ## ESTABLISH MATRICES

  Var <- matrix(0, 4, 4) # Variances - covariances matrix

  Var[1, 1] <- ((vals$p10 + vals$p11) * (vals$q10 + vals$q11)) / (vals$n *
    (vals$p10 + vals$p11 + vals$q10 + vals$q11)^3)

  Var[1, 2] <- (vals$p01 * vals$p10 * vals$q11 + vals$p11 * (vals$q01 *
    (vals$q10 + vals$q11) + vals$q11 *
      (vals$p01 + vals$p10 + vals$p11 + vals$q10 + vals$q11))) /
    (vals$n * (vals$p01 + vals$p11 + vals$q01 + vals$q11)^2 *
      (vals$p10 + vals$p11 + vals$q10 + vals$q11)^2)

  Var[1, 3] <- 0

  Var[1, 4] <- -(vals$p00 * (vals$p10 + vals$p11) * vals$q10 + vals$p10 *
    vals$q10 * (vals$p10 + vals$p11 + vals$q00 + vals$q10) + vals$p10
    * (vals$q00 + vals$q10) * vals$q11) / (vals$n * (vals$p00 +
    vals$p10 + vals$q00 + vals$q10)^2 * (vals$p10 + vals$p11 +
    vals$q10 + vals$q11)^2)

  Var[2, 1] <- (vals$p01 * vals$p10 * vals$q11 + vals$p11 * (vals$q01 *
    (vals$q10 + vals$q11) + vals$q11 * (vals$p01 + vals$p10 + vals$p11
      + vals$q10 + vals$q11))) / (vals$n * (vals$p01 + vals$p11 +
    vals$q01 + vals$q11)^2 * (vals$p10 + vals$p11 + vals$q10 +
    vals$q11)^2)

  Var[2, 2] <- ((vals$p01 + vals$p11) * (vals$q01 + vals$q11)) / (vals$n *
    (vals$p01 + vals$p11 + vals$q01 + vals$q11)^3)

  Var[2, 3] <- -(vals$p00 * (vals$p01 + vals$p11) * vals$q01 + vals$p01 *
    vals$q01 * (vals$p01 + vals$p11 + vals$q00 + vals$q01) + vals$p01
    * (vals$q00 + vals$q01) * vals$q11) / (vals$n * (vals$p00 +
    vals$p01 + vals$q00 + vals$q01)^2 * (vals$p01 + vals$p11 +
    vals$q01 + vals$q11)^2)

  Var[2, 4] <- 0

  Var[3, 1] <- 0

  Var[3, 2] <- Var[2, 3]

  Var[3, 3] <- ((vals$p00 + vals$p01) * (vals$q00 + vals$q01)) / (vals$n *
    (vals$p00 + vals$p01 + vals$q00 + vals$q01)^3)

  Var[3, 4] <- (vals$q00 * (vals$p00^2 + vals$p01 * vals$p10 + vals$p00 *
    (vals$p01 + vals$p10 + vals$q00 + vals$q01)) + vals$p00 *
    (vals$q00 + vals$q01) * vals$q10) / (vals$n * (vals$p00 + vals$p01
    + vals$q00 + vals$q01)^2 * (vals$p00 + vals$p10 + vals$q00 +
    vals$q10)^2)

  Var[4, 1] <- Var[1, 4]

  Var[4, 2] <- 0

  Var[4, 3] <- Var[3, 4]

  Var[4, 4] <- ((vals$p00 + vals$p10) * (vals$q00 + vals$q10)) / (vals$n *
    (vals$p00 + vals$p10 + vals$q00 + vals$q10)^3)

  phi <- matrix(0, 2, 4)

  phi[1, 1] <- 1
  phi[1, 2] <- -1

  phi[2, 3] <- 1
  phi[2, 4] <- -1

  PV <- as.matrix(c(vals$PPV1, vals$PPV2, vals$NPV1, vals$NPV2))

  dim(PV) <- c(1, 4)

  sigmaPV <- phi %*% Var %*% t(phi)

  if ((vals$s10 == vals$s01) && (vals$r10 == vals$r01)) {
    Q2 <- NaN
  } else if (any(is.na(PV) | is.nan(det(sigmaPV)) | det(sigmaPV == 0))) {
    Q2 <- NaN
  } else {
    Q2 <- as.numeric(PV %*% t(phi) %*% solve(sigmaPV) %*% phi %*% t(PV))
  }

  globalpvalue2 <- as.numeric(1 - pchisq(Q2, 2, ...))

  PPVp <- (2 * vals$s11 + vals$s10 + vals$s01) /
    (2 * vals$n11 + vals$n10 + vals$n01)

  NPVp <- (2 * vals$r00 + vals$r01 + vals$r10) /
    (2 * vals$n00 + vals$n01 + vals$n10)

  CPPVp <- (vals$s11 * (1 - PPVp)^2 + vals$r11 * PPVp^2) /
    (2 * vals$n11 + vals$n10 + vals$n01)

  CNPVp <- (vals$s00 * NPVp^2 + vals$r00 * (1 - NPVp)^2) /
    (2 * vals$n00 + vals$n01 + vals$n10)

  T1 <- (vals$PPV1 - vals$PPV2)^2 / ((PPVp * (1 - PPVp) - 2 * CPPVp) *
    ((1 / (vals$n10 + vals$n11)) + (1 / (vals$n01 + vals$n11))))
  T2 <- (vals$NPV1 - vals$NPV2)^2 / ((NPVp * (1 - NPVp) - 2 * CNPVp) *
    ((1 / (vals$n01 + vals$n00)) + (1 / (vals$n10 + vals$n00))))

  pvalue5 <- (1 - pchisq(T1, 1, ...))
  pvalue6 <- (1 - pchisq(T2, 1, ...))

  return(list(
    glob.t = Q2, glob.p = globalpvalue2,
    ind.t1 = T1, ind.t2 = T2,
    ind.p1 = pvalue5, ind.p2 = pvalue6
  ))
}

#' @title output.lr
#'
#' @description Performs statistical inference tests on the likelihood ratios
#' of two binary diagnostic tests.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param ... Rarely needs to be used. Allows additional arguments to be passed
#' to internal functions.
#'
#' @return A list object containing test statistics and p-values for global and
#' individual hypothesis tests of likelihood ratios.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @importFrom stats "pchisq" "pnorm"

output.lr <- function(vals, ...) {
  ## CHECK ARGUMENTS

  if (!inherits(vals, "vals.2test")) {
    stop("Argument must be of \"vals.2test\" class.")
  }

  ## ESTABLISH MATRICES

  logposw <- log(vals$PLR1 / vals$PLR2)
  lognegw <- log(vals$NLR1 / vals$NLR2)
  logwmat <- as.matrix(c(logposw, lognegw))

  mat1 <- matrix(0, 2, 8)

  mat1[1, 1] <- 1 / (vals$p10 + vals$p11) - 1 / (vals$p01 + vals$p11)
  mat1[1, 2] <- 1 / (vals$p10 + vals$p11)
  mat1[1, 3] <- -1 / (vals$p01 + vals$p11)
  mat1[1, 4] <- 0

  mat1[1, 5] <- 1 / (vals$q01 + vals$q11) - 1 / (vals$q10 + vals$q11)
  mat1[1, 6] <- -1 / (vals$q10 + vals$q11)
  mat1[1, 7] <- 1 / (vals$q01 + vals$q11)
  mat1[1, 8] <- 0

  mat1[2, 1] <- 0
  mat1[2, 2] <- -1 / (vals$p00 + vals$p10)
  mat1[2, 3] <- 1 / (vals$p00 + vals$p01)
  mat1[2, 4] <- 1 / (vals$p00 + vals$p01) - 1 / (vals$p00 + vals$p10)

  mat1[2, 5] <- 0
  mat1[2, 6] <- 1 / (vals$q00 + vals$q10)
  mat1[2, 7] <- -1 / (vals$q00 + vals$q01)
  mat1[2, 8] <- 1 / (vals$q00 + vals$q10) - 1 / (vals$q00 + vals$q01)

  vec1 <- vector("numeric", 8)

  vec1[1] <- vals$p11
  vec1[2] <- vals$p10
  vec1[3] <- vals$p01
  vec1[4] <- vals$p00

  vec1[5] <- vals$q11
  vec1[6] <- vals$q10
  vec1[7] <- vals$q01
  vec1[8] <- vals$q00

  mat2 <- matrix(0, 8, 8)

  mat2[1, 1] <- vals$p11
  mat2[2, 2] <- vals$p10
  mat2[3, 3] <- vals$p01
  mat2[4, 4] <- vals$p00

  mat2[5, 5] <- vals$q11
  mat2[6, 6] <- vals$q10
  mat2[7, 7] <- vals$q01
  mat2[8, 8] <- vals$q00

  sigma1 <- matrix(0, 8, 8)
  sigma1 <- (1 / vals$n) * (mat2 - vec1 %*% t(vec1))

  sigmaLR <- matrix(0, 2, 2)
  sigmaLR <- mat1 %*% sigma1 %*% t(mat1)

  ## GLOBAL TESTS

  if ((vals$s10 == vals$s01) && (vals$r10 == vals$r01)) {
    Q3 <- NaN
  } else if (is.nan(det(sigmaLR)) | det(sigmaLR) == 0) {
    Q3 <- NaN
  } else {
    Q3 <- as.numeric(t(logwmat) %*% solve(sigmaLR) %*% logwmat)
  }

  globalpvalue3 <- as.numeric(1 - pchisq(Q3, 2, ...))

  ## INDIVIDUAL TESTS

  z1 <- abs(logposw) / sqrt(sigmaLR[1, 1])
  z2 <- abs(lognegw) / sqrt(sigmaLR[2, 2])

  pvalue7 <- 2 * (1 - pnorm(z1, 0, 1, ...))
  pvalue8 <- 2 * (1 - pnorm(z2, 0, 1, ...))

  return(list(
    glob.t = Q3, glob.p = globalpvalue3,
    ind.t1 = z1, ind.t2 = z2,
    ind.p1 = pvalue7, ind.p2 = pvalue8
  ))
}

#-------------------------------------------------------------------------------
# PRODUCING MATRICES OF RESULTS
#-------------------------------------------------------------------------------

#' @title matrixify
#'
#' @description Produces a matrix containing point estimates, standard errors
#' and upper and lower confidence bounds for related outputs (eg. sensitivity
#' and specificity) of one or two binary diagnostic tests.
#'
#' @param vals A list object containing all values required for calculations
#' with class `vals.1test` or `vals.2test`. These objects are output by the
#' `values.1test()` and `values.2test()` function.
#' @param rows A vector of length two giving the names of the related tests
#' (eg. sensitivity and specificity).
#' @param test.names A vector of length two giving the names of the two
#' different binary diagnostic tests. This argument is not relevant when testing
#' a single binary diagnostic test.
#' @param dp Number of decimal places of output. Defaults to 1.
#'
#' @return Either a matrix or list of matrices displaying point estimates,
#' standard errors and upper and lower confidence bounds for related outputs
#' (eg. sensitivity and specificity) of one or two binary diagnostic tests.
#'
#' @keywords internal
#'
#' @noRd
#'
matrixify <- function(vals, rows = c("Row 1", "Row 2"),
                      test.names = c("Test 1", "Test 2"), dp = 3) {
  if (inherits(vals, "conf.1t")) {
    mat <- matrix(rep(0, 8),
      nrow = 2, ncol = 4,
      dimnames = list(
        c(rows[1], rows[2]),
        c("Estimate", "SE", "Lower CI", "Upper CI")
      )
    )

    mat[1, 1] <- round(vals[[1]][[1]], dp) * 100
    mat[1, 2] <- round(vals[[1]][[2]], dp) * 100
    mat[1, 3] <- round(vals[[1]][[3]], dp) * 100
    mat[1, 4] <- round(vals[[1]][[4]], dp) * 100

    mat[2, 1] <- round(vals[[2]][[1]], dp) * 100
    mat[2, 2] <- round(vals[[2]][[2]], dp) * 100
    mat[2, 3] <- round(vals[[2]][[3]], dp) * 100
    mat[2, 4] <- round(vals[[2]][[4]], dp) * 100

    return(mat)
  } else if (inherits(vals, "conf.1t.lr")) {
    dp <- dp - 2
    mat <- matrix(rep(0, 8),
      nrow = 2, ncol = 4,
      dimnames = list(
        c(rows[1], rows[2]),
        c("Estimate", "SE", "Lower CI", "Upper CI")
      )
    )

    mat[1, 1] <- round(vals[[1]][[1]], dp)
    mat[1, 2] <- round(vals[[1]][[2]], dp)
    mat[1, 3] <- round(vals[[1]][[3]], dp)
    mat[1, 4] <- round(vals[[1]][[4]], dp)

    mat[2, 1] <- round(vals[[2]][[1]], dp)
    mat[2, 2] <- round(vals[[2]][[2]], dp)
    mat[2, 3] <- round(vals[[2]][[3]], dp)
    mat[2, 4] <- round(vals[[2]][[4]], dp)

    return(mat)
  } else if (inherits(vals, "conf.2t")) {
    mat1 <- matrix(rep(0, 8),
      nrow = 2, ncol = 4,
      dimnames = list(
        c(rows[1], rows[2]),
        c("Estimate", "SE", "Lower CI", "Upper CI")
      )
    )

    mat1[1, 1] <- round(vals[[1]][[1]], dp) * 100
    mat1[1, 2] <- round(vals[[1]][[2]], dp) * 100
    mat1[1, 3] <- round(vals[[1]][[3]], dp) * 100
    mat1[1, 4] <- round(vals[[1]][[4]], dp) * 100

    mat1[2, 1] <- round(vals[[3]][[1]], dp) * 100
    mat1[2, 2] <- round(vals[[3]][[2]], dp) * 100
    mat1[2, 3] <- round(vals[[3]][[3]], dp) * 100
    mat1[2, 4] <- round(vals[[3]][[4]], dp) * 100

    mat2 <- matrix(rep(0, 8),
      nrow = 2, ncol = 4,
      dimnames = list(
        c(rows[1], rows[2]),
        c("Estimate", "SE", "Lower CI", "Upper CI")
      )
    )

    mat2[1, 1] <- round(vals[[2]][[1]], dp) * 100
    mat2[1, 2] <- round(vals[[2]][[2]], dp) * 100
    mat2[1, 3] <- round(vals[[2]][[3]], dp) * 100
    mat2[1, 4] <- round(vals[[2]][[4]], dp) * 100

    mat2[2, 1] <- round(vals[[4]][[1]], dp) * 100
    mat2[2, 2] <- round(vals[[4]][[2]], dp) * 100
    mat2[2, 3] <- round(vals[[4]][[3]], dp) * 100
    mat2[2, 4] <- round(vals[[4]][[4]], dp) * 100

    mat <- list(mat1, mat2)
    names(mat) <- test.names

    return(mat)
  } else if (inherits(vals, "conf.2t.lr")) {
    dp <- dp - 2
    mat1 <- matrix(rep(0, 8),
      nrow = 2, ncol = 4,
      dimnames = list(
        c(rows[1], rows[2]),
        c("Estimate", "SE", "Lower CI", "Upper CI")
      )
    )

    mat1[1, 1] <- round(vals[[1]][[1]], dp)
    mat1[1, 2] <- round(vals[[1]][[2]], dp)
    mat1[1, 3] <- round(vals[[1]][[3]], dp)
    mat1[1, 4] <- round(vals[[1]][[4]], dp)

    mat1[2, 1] <- round(vals[[3]][[1]], dp)
    mat1[2, 2] <- round(vals[[3]][[2]], dp)
    mat1[2, 3] <- round(vals[[3]][[3]], dp)
    mat1[2, 4] <- round(vals[[3]][[4]], dp)

    mat2 <- matrix(rep(0, 8),
      nrow = 2, ncol = 4,
      dimnames = list(
        c(rows[1], rows[2]),
        c("Estimate", "SE", "Lower CI", "Upper CI")
      )
    )

    mat2[1, 1] <- round(vals[[2]][[1]], dp)
    mat2[1, 2] <- round(vals[[2]][[2]], dp)
    mat2[1, 3] <- round(vals[[2]][[3]], dp)
    mat2[1, 4] <- round(vals[[2]][[4]], dp)

    mat2[2, 1] <- round(vals[[4]][[1]], dp)
    mat2[2, 2] <- round(vals[[4]][[2]], dp)
    mat2[2, 3] <- round(vals[[4]][[3]], dp)
    mat2[2, 4] <- round(vals[[4]][[4]], dp)

    mat <- list(mat1, mat2)
    names(mat) <- test.names

    return(mat)
  } else if (inherits(vals, "prev")) {
    mat <- matrix(rep(0, 4),
      nrow = 1, ncol = 4,
      dimnames = list(
        "Prevalence",
        c("Estimate", "SE", "Lower CI", "Upper CI")
      )
    )

    mat[1, 1] <- round(vals[[1]], dp) * 100
    mat[1, 2] <- round(vals[[2]], dp) * 100
    mat[1, 3] <- round(vals[[3]], dp) * 100
    mat[1, 4] <- round(vals[[4]], dp) * 100

    return(mat)
  }
}

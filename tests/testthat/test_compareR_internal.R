#-------------------------------------------------------------------------------
# CHECK.DF
#-------------------------------------------------------------------------------

test_that("check.df", {
  # Test case 1: Valid data frame
  df1 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_silent(check.df(df1))

  # Test case 2: Data frame with factors
  df2 <- data.frame(
    col1 = as.factor(c("positive", "pos", "p", "yes", "y", "+", "1")),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_no_error(check.df(df2))

  # Test case 3: Data frame with NAs
  df3 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", NA, "-", "0", "2")
  )
  expect_error(check.df(df3), ".*NAs are not supported\\..*")

  # Test case 4: Data frame with NA synonyms
  df4 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "na", "-", "0", "2")
  )
  expect_error(check.df(df4), ".*NAs are not supported\\..*")

  # Test case 5: Data frame with coding errors
  df5 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2"),
    col3 = c("invalid", "value", "here", "yes", "no", "yes", "+")
  )
  expect_error(check.df(df5), ".*Coding errors exist\\..*")

  # Test case 6: Data frame has too many columns
  df6 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2"),
    col3 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col4 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_error(check.df(df6), ".*with two or three*")

  # Test case 7: Valid matrix
  mat <- matrix(
    sample(c(0, 1), 10, replace = TRUE),
    ncol = 2
  )
  expect_silent(check.df(mat))

  # Test case 8: Invalid matrix
  mat <- matrix(
    sample(3:4, 10, replace = TRUE),
    ncol = 2
  )
  expect_error(check.df(mat), ".*Coding errors exist\\..*")

  # Test case 9: Invalid object
  vec <- c(0, 1, 0, 1, 0)
  expect_error(
    check.df(vec),
    "Data should be provided as a data frame or matrix."
  )
})

#-------------------------------------------------------------------------------
# RECODER
#-------------------------------------------------------------------------------

test_that("recoder", {
  # Test case 1: Valid data frame (characters)
  df1 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_s3_class(recoder(df1), "data.frame")
  expect_true(all(sapply(recoder(df1), is.integer)))
  expect_true(all(sapply(recoder(df1), function(x) all(x %in% c(0, 1)))))

  # Test case 2: Valid data frame (numeric)
  df2 <- data.frame(
    col1 = c(0, 1, 1, 0, 1),
    col2 = c(1, 1, 0, 0, 1)
  )
  expect_s3_class(recoder(df1), "data.frame")
  expect_true(all(sapply(recoder(df1), is.integer)))
  expect_true(all(sapply(recoder(df1), function(x) all(x %in% c(0, 1)))))

  # ARGUMENT VALIDITY (CHECK.DF)

  df1 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_silent(check.df(df1))

  df2 <- data.frame(
    col1 = as.factor(c("positive", "pos", "p", "yes", "y", "+", "1")),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_no_error(check.df(df2))

  df3 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", NA, "-", "0", "2")
  )
  expect_error(check.df(df3), ".*NAs are not supported\\..*")

  df4 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "na", "-", "0", "2")
  )
  expect_error(check.df(df4), ".*NAs are not supported\\..*")

  df5 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2"),
    col3 = c("invalid", "value", "here", "yes", "no", "yes", "+")
  )
  expect_error(check.df(df5), ".*Coding errors exist\\..*")

  df6 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2"),
    col3 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col4 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_error(check.df(df6), ".*with two or three*")

  mat <- matrix(
    sample(c(0, 1), 10, replace = TRUE),
    ncol = 2
  )
  expect_silent(check.df(mat))

  mat <- matrix(
    sample(3:4, 10, replace = TRUE),
    ncol = 2
  )
  expect_error(check.df(mat), ".*Coding errors exist\\..*")

  vec <- c(0, 1, 0, 1, 0)
  expect_error(
    check.df(vec),
    "Data should be provided as a data frame or matrix."
  )
})

#-------------------------------------------------------------------------------
# VALUES.1TEST
#-------------------------------------------------------------------------------

test_that("values.1test function tests", {
  # Test case 1: Correct input data frame
  df1 <- data.frame(
    col1 = as.integer(c(1, 0, 1, 0)),
    col2 = as.integer(c(1, 1, 0, 0))
  )
  result1 <- values.1test(df1)
  expect_type(result1$s1, "integer")
  expect_gte(result1$s1, 0)
  expect_type(result1$s0, "integer")
  expect_gte(result1$s0, 0)
  expect_type(result1$r1, "integer")
  expect_gte(result1$r1, 0)
  expect_type(result1$r0, "integer")
  expect_gte(result1$r0, 0)
  expect_type(result1$ss, "integer")
  expect_gte(result1$ss, 0)
  expect_type(result1$rr, "integer")
  expect_gte(result1$rr, 0)
  expect_type(result1$n1, "integer")
  expect_gte(result1$n1, 0)
  expect_type(result1$n0, "integer")
  expect_gte(result1$n0, 0)
  expect_type(result1$n, "integer")
  expect_gte(result1$n, result1$ss)
  expect_gte(result1$n, result1$rr)
  expect_gte(result1$n, result1$n1)
  expect_gte(result1$n, result1$n0)
  expect_gte(result1$p1, 0)
  expect_lte(result1$p1, 1)
  expect_gte(result1$p0, 0)
  expect_lte(result1$p0, 1)
  expect_gte(result1$q1, 0)
  expect_lte(result1$q1, 1)
  expect_gte(result1$q0, 0)
  expect_lte(result1$q0, 1)
  expect_gte(result1$prev, 0)
  expect_lte(result1$prev, 1)
  expect_gte(result1$qrev, 0)
  expect_lte(result1$qrev, 1)
  expect_gte(result1$qrev, 0)
  expect_lte(result1$qrev, 1)
  expect_gte(result1$qrev, 0)
  expect_lte(result1$qrev, 1)
  expect_gte(result1$Se1, 0)
  expect_lte(result1$Se1, 1)
  expect_gte(result1$Sp1, 0)
  expect_lte(result1$Sp1, 1)
  expect_gte(result1$PPV1, 0)
  expect_lte(result1$PPV1, 1)
  expect_gte(result1$NPV1, 0)
  expect_lte(result1$NPV1, 1)
  expect_type(result1$PLR1, "double")
  expect_type(result1$NLR1, "double")


  # Test case 2: Incorrect number of columns in input data frame
  df2 <- data.frame(
    col1 = as.integer(c(1, 0, 1, 0, 1)),
    col2 = as.integer(c(1, 1, 0, 0, 1)),
    col3 = as.integer(c(1, 1, 1, 0, 0))
  )
  expect_error(
    values.1test(df2),
    "Please provide data as a data frame with two columns."
  )
})

#-------------------------------------------------------------------------------
# VALUES.2TEST
#-------------------------------------------------------------------------------

test_that("values.2test function tests", {
  # Test case 1: Correct input data frame

  df1 <- data.frame(
    col1 = as.integer(c(1, 0, 1, 0)),
    col2 = as.integer(c(1, 1, 0, 0)),
    col3 = as.integer(c(0, 1, 1, 0))
  )
  result1 <- values.2test(df1)
  expect_type(result1$s11, "integer")
  expect_gte(result1$s11, 0)
  expect_type(result1$s10, "integer")
  expect_gte(result1$s10, 0)
  expect_type(result1$s01, "integer")
  expect_gte(result1$s01, 0)
  expect_type(result1$s00, "integer")
  expect_gte(result1$s00, 0)
  expect_type(result1$r11, "integer")
  expect_gte(result1$r11, 0)
  expect_type(result1$r10, "integer")
  expect_gte(result1$r10, 0)
  expect_type(result1$r01, "integer")
  expect_gte(result1$r01, 0)
  expect_type(result1$r00, "integer")
  expect_gte(result1$r00, 0)
  expect_type(result1$ss, "integer")
  expect_gte(result1$ss, 0)
  expect_type(result1$rr, "integer")
  expect_gte(result1$rr, 0)
  expect_type(result1$n11, "integer")
  expect_gte(result1$n11, 0)
  expect_type(result1$n10, "integer")
  expect_gte(result1$n10, 0)
  expect_type(result1$n01, "integer")
  expect_gte(result1$n01, 0)
  expect_type(result1$n00, "integer")
  expect_gte(result1$n00, 0)
  expect_type(result1$n, "integer")
  expect_gte(result1$n, result1$ss)
  expect_gte(result1$n, result1$rr)
  expect_gte(result1$n, result1$n11)
  expect_gte(result1$n, result1$n10)
  expect_gte(result1$n, result1$n01)
  expect_gte(result1$n, result1$n00)
  expect_gte(result1$p11, 0)
  expect_lte(result1$p11, 1)
  expect_gte(result1$p10, 0)
  expect_lte(result1$p10, 1)
  expect_gte(result1$p01, 0)
  expect_lte(result1$p01, 1)
  expect_gte(result1$p00, 0)
  expect_lte(result1$p00, 1)
  expect_gte(result1$q11, 0)
  expect_lte(result1$q11, 1)
  expect_gte(result1$q10, 0)
  expect_lte(result1$q10, 1)
  expect_gte(result1$q01, 0)
  expect_lte(result1$q01, 1)
  expect_gte(result1$q00, 0)
  expect_lte(result1$q00, 1)
  expect_gte(result1$prev, 0)
  expect_lte(result1$prev, 1)
  expect_gte(result1$qrev, 0)
  expect_lte(result1$qrev, 1)
  expect_gte(result1$qrev, 0)
  expect_lte(result1$qrev, 1)
  expect_gte(result1$qrev, 0)
  expect_lte(result1$qrev, 1)
  expect_gte(result1$Se1, 0)
  expect_lte(result1$Se1, 1)
  expect_gte(result1$Se2, 0)
  expect_lte(result1$Se2, 1)
  expect_gte(result1$Sp1, 0)
  expect_lte(result1$Sp1, 1)
  expect_gte(result1$Sp2, 0)
  expect_lte(result1$Sp2, 1)
  expect_gte(result1$PPV1, 0)
  expect_lte(result1$PPV1, 1)
  expect_gte(result1$PPV2, 0)
  expect_lte(result1$PPV2, 1)
  expect_gte(result1$NPV1, 0)
  expect_lte(result1$NPV1, 1)
  expect_gte(result1$NPV2, 0)
  expect_lte(result1$NPV2, 1)
  expect_type(result1$PLR1, "double")
  expect_type(result1$PLR2, "double")
  expect_type(result1$NLR1, "double")
  expect_type(result1$NLR2, "double")

  # Test case 2: Incorrect number of columns in input data frame

  df2 <- data.frame(
    col1 = as.integer(c(1, 0, 1, 0, 1)),
    col2 = as.integer(c(1, 1, 0, 0, 1))
  )
  expect_error(
    values.2test(df2),
    "Please provide data as a data frame with three columns."
  )
})

#-------------------------------------------------------------------------------
# DISP.CONT
#-------------------------------------------------------------------------------

test_that("disp.cont function tests", {
  # Test 1: Correct values.1test without margins

  df1 <- data.frame(
    col1 = as.integer(c(1, 0, 1, 0)),
    col2 = as.integer(c(1, 1, 0, 0))
  )
  results1 <- values.1test(df1)
  expect_type(disp.cont(results1), "integer")
  expect_true(all(disp.cont(results1)) >= 0)
  expect_equal(disp.cont(results1),
    matrix(c(1, 1, 1, 1), ncol = 2),
    ignore_attr = TRUE
  )

  # Test 2: Correct values.1test without margins

  expect_type(disp.cont(results1, margins = TRUE), "double")
  expect_true(all(disp.cont(results1)) >= 0)
  expect_equal(disp.cont(results1, margins = TRUE),
    addmargins(matrix(c(1, 1, 1, 1), ncol = 2)),
    ignore_attr = TRUE
  )

  # Test 3: Correct values.2test without margins

  df2 <- data.frame(
    col1 = as.integer(c(1, 0, 1, 0, 1, 0, 1, 0)),
    col2 = as.integer(c(1, 1, 0, 0, 1, 1, 0, 0)),
    col3 = as.integer(c(1, 1, 1, 1, 0, 0, 0, 0))
  )
  results2 <- values.2test(df2)
  expect_type(disp.cont(results2), "list")
  expect_type(disp.cont(results2)[[1]], "integer")
  expect_type(disp.cont(results2)[[2]], "integer")
  expect_true(all(disp.cont(results2)[[1]] >= 0))
  expect_true(all(disp.cont(results2)[[2]] >= 0))
  expect_equal(disp.cont(results2)[[1]], matrix(c(2, 2, 2, 2), ncol = 2),
    ignore_attr = TRUE
  )
  expect_equal(disp.cont(results2)[[2]], matrix(c(2, 2, 2, 2), ncol = 2),
    ignore_attr = TRUE
  )
  expect_equal(disp.cont(results2)[[3]], matrix(c(1, 1, 1, 1), ncol = 2),
               ignore_attr = TRUE
  )
  expect_equal(disp.cont(results2)[[4]], matrix(c(1, 1, 1, 1), ncol = 2),
               ignore_attr = TRUE
  )

  # Test 4: Correct values.2test without margins

  expect_type(disp.cont(results2, margins = TRUE), "list")
  expect_type(disp.cont(results2, margins = TRUE)[[1]], "double")
  expect_type(disp.cont(results2, margins = TRUE)[[2]], "double")
  expect_true(all(disp.cont(results2, margins = TRUE)[[1]] >= 0))
  expect_true(all(disp.cont(results2, margins = TRUE)[[2]] >= 0))
  expect_equal(disp.cont(results2, margins = TRUE)[[1]],
    addmargins(matrix(c(2, 2, 2, 2), ncol = 2)),
    ignore_attr = TRUE
  )
  expect_equal(disp.cont(results2, margins = TRUE)[[2]],
    addmargins(matrix(c(2, 2, 2, 2), ncol = 2)),
    ignore_attr = TRUE
  )
  expect_equal(disp.cont(results2, margins = TRUE)[[3]],
               addmargins(matrix(c(1, 1, 1, 1), ncol = 2)),
               ignore_attr = TRUE
  )
  expect_equal(disp.cont(results2, margins = TRUE)[[4]],
               addmargins(matrix(c(1, 1, 1, 1), ncol = 2)),
               ignore_attr = TRUE
  )

  # Test 5: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(disp.cont(
    l1,
    "Argument must be of \"vals.1test\" or \"vals.2test\" class."
  ))

  # Test 6: Non-list object without class

  v1 <- c(TP = 49, FN = 17, FP = 2, TN = 113)
  expect_error(disp.cont(
    v1,
    "Argument must be of \"vals.1test\" or \"vals.2test\" class."
  ))

  # Edge case 1: Incorrect list object with correct class

  l2 <- list(TP = 49, FN = 17, FP = 2, TN = 113)
  class(l2) <- "vals.1test"
  expect_error(disp.cont(l2))

  # Edge case 2: Non-list object with correct class

  v2 <- c(TP = 49, FN = 17, FP = 2, TN = 113)
  class(v2) <- "vals.1test"
  expect_error(disp.cont(v2))
})

#-------------------------------------------------------------------------------
# YU.INT
#-------------------------------------------------------------------------------

test_that("yu.int function tests", {
  # Test 1: Correct values

  n <- as.integer(100)
  z <- qnorm(1 - 0.05 / 2)
  est <- 0.5
  output <- yu.int(n, z, est)

  expect_gte(output$lower, 0)
  expect_lte(output$upper, 1)
  expect_equal(output$lower, 0.4038315, tolerance = 1e-06)
  expect_equal(output$upper, 0.5961685, tolerance = 1e-06)

  # Test 2: n is negative

  n <- as.integer(-100)
  z <- qnorm(1 - 0.05 / 2)
  est <- 0.5

  expect_error(yu.int(n, z, est), "n must be a non-negative integer.")

  # Test 3: n is not an integer

  n <- 100
  z <- qnorm(1 - 0.05 / 2)
  est <- 0.5

  expect_error(yu.int(n, z, est), "n must be a non-negative integer.")

  # Test 4: z is negative

  n <- as.integer(100)
  z <- -qnorm(1 - 0.05 / 2)
  est <- 0.5

  expect_error(yu.int(n, z, est), "z must not be negative.")

  # Test 5: Point estimate is not between 0 and 1

  n <- as.integer(100)
  z <- qnorm(1 - 0.05 / 2)
  est <- 50

  expect_error(
    yu.int(n, z, est),
    "Point estimate must be between 0 and 1, inclusive."
  )

  # Edge case 1: low n, point estimate close to 0 (will produce LCI < 0)

  n <- as.integer(50)
  z <- qnorm(1 - 0.05 / 2)
  est <- 0.00001
  output <- yu.int(n, z, est)

  expect_equal(output$lower, 0)
  expect_lt(output$upper, 1)

  # Edge case 2: low n, point estimate close to 1 (will produce UCI < 0)

  n <- as.integer(50)
  z <- qnorm(1 - 0.05 / 2)
  est <- 0.99999
  output <- yu.int(n, z, est)

  expect_gt(output$lower, 0)
  expect_equal(output$upper, 1)

  # Edge case 3: Point estimate is NA/NaN

  n <- as.integer(50)
  z <- qnorm(1 - 0.05 / 2)
  est <- NA
  output <- yu.int(n, z, est)

  expect_equal(output$lower, 0)
  expect_equal(output$upper, 1)
})

#-------------------------------------------------------------------------------
# CONF.PREV
#-------------------------------------------------------------------------------

test_that("conf.prev function tests", {
  # Test 1: Correct values (default non-data arguments)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- conf.prev(dat)

  expect_type(output1, "list")
  expect_equal(output1$est, 0.6666667, tolerance = 1e-06)
  expect_equal(output1$SE, 0.01924501, tolerance = 1e-06)
  expect_equal(output1$ci.lower, 0.6280689, tolerance = 1e-06)
  expect_equal(output1$ci.upper, 0.7032975, tolerance = 1e-06)
  expect_lt(output1$ci.lower, output1$est)
  expect_gt(output1$ci.upper, output1$est)

  # Test 2: Correct values (custom non-data arguments)

  output2 <- conf.prev(dat, alpha = 0.1)

  expect_type(output2, "list")
  expect_equal(output2$est, 0.6666667, tolerance = 1e-06)
  expect_equal(output2$SE, 0.01924501, tolerance = 1e-06)
  expect_equal(output2$ci.lower, 0.6343637, tolerance = 1e-06)
  expect_equal(output2$ci.upper, 0.6975496, tolerance = 1e-06)
  expect_lt(output2$ci.lower, output2$est)
  expect_gt(output2$ci.upper, output2$est)
  expect_lt(
    output2$ci.upper - output2$ci.lower,
    output1$ci.upper - output1$ci.lower
  )

  # Test 3: Unacceptable alpha value

  expect_error(
    conf.prev(dat, alpha = 1.1),
    "Alpha must be a value between 0 and 1, inclusive."
  )

  # Test 4: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(conf.prev(
    l1,
    "Argument must be of \"vals.1test\" or \"vals.2test\" class."
  ))
})

#-------------------------------------------------------------------------------
# CONF.ACC
#-------------------------------------------------------------------------------

test_that("conf.acc function tests", {
  # Test 1: Correct values (default non-data arguments)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- conf.acc(dat)

  expect_type(output1, "list")
  expect_equal(output1$test1.se$est, 0.75, tolerance = 1e-06)
  expect_equal(output1$test1.se$SE, 0.02165064, tolerance = 1e-06)
  expect_equal(output1$test1.se$ci.lower, 0.7054952, tolerance = 1e-06)
  expect_equal(output1$test1.se$ci.upper, 0.7900933, tolerance = 1e-06)
  expect_lt(output1$test1.se$ci.lower, output1$test1.se$est)
  expect_gt(output1$test1.se$ci.upper, output1$test1.se$est)

  expect_equal(output1$test2.se$est, 0.7, tolerance = 1e-06)
  expect_equal(output1$test2.se$SE, 0.02291288, tolerance = 1e-06)
  expect_equal(output1$test2.se$ci.lower, 0.6535006, tolerance = 1e-06)
  expect_equal(output1$test2.se$ci.upper, 0.7429702, tolerance = 1e-06)
  expect_lt(output1$test2.se$ci.lower, output1$test2.se$est)
  expect_gt(output1$test2.se$ci.upper, output1$test2.se$est)

  expect_equal(output1$test1.sp$est, 0.725, tolerance = 1e-06)
  expect_equal(output1$test1.sp$SE, 0.03157333, tolerance = 1e-06)
  expect_equal(output1$test1.sp$ci.lower, 0.6596239, tolerance = 1e-06)
  expect_equal(output1$test1.sp$ci.upper, 0.7825103, tolerance = 1e-06)
  expect_lt(output1$test1.sp$ci.lower, output1$test1.sp$est)
  expect_gt(output1$test1.sp$ci.upper, output1$test1.sp$est)

  expect_equal(output1$test2.sp$est, 0.775, tolerance = 1e-06)
  expect_equal(output1$test2.sp$SE, 0.02952753, tolerance = 1e-06)
  expect_equal(output1$test2.sp$ci.lower, 0.7126344, tolerance = 1e-06)
  expect_equal(output1$test2.sp$ci.upper, 0.8277519, tolerance = 1e-06)
  expect_lt(output1$test2.sp$ci.lower, output1$test2.sp$est)
  expect_gt(output1$test2.sp$ci.upper, output1$test2.sp$est)

  # Test 2: Correct values (custom non-data arguments)

  output2 <- conf.acc(dat, alpha = 0.1)

  expect_type(output2, "list")
  expect_equal(output2$test1.se$est, 0.75, tolerance = 1e-06)
  expect_equal(output2$test1.se$SE, 0.02165064, tolerance = 1e-06)
  expect_equal(output2$test1.se$ci.lower, 0.7128741, tolerance = 1e-06)
  expect_equal(output2$test1.se$ci.upper, 0.7839382, tolerance = 1e-06)
  expect_lt(output2$test1.se$ci.lower, output2$test1.se$est)
  expect_gt(output2$test1.se$ci.upper, output2$test1.se$est)

  expect_equal(output2$test2.se$est, 0.7, tolerance = 1e-06)
  expect_equal(output2$test2.se$SE, 0.02291288, tolerance = 1e-06)
  expect_equal(output2$test2.se$ci.lower, 0.6611394, tolerance = 1e-06)
  expect_equal(output2$test2.se$ci.upper, 0.7363105, tolerance = 1e-06)
  expect_lt(output2$test2.se$ci.lower, output2$test2.se$est)
  expect_gt(output2$test2.se$ci.upper, output2$test2.se$est)

  expect_equal(output2$test1.sp$est, 0.725, tolerance = 1e-06)
  expect_equal(output2$test1.sp$SE, 0.03157333, tolerance = 1e-06)
  expect_equal(output2$test1.sp$ci.lower, 0.6704771, tolerance = 1e-06)
  expect_equal(output2$test1.sp$ci.upper, 0.7738233, tolerance = 1e-06)
  expect_lt(output2$test1.sp$ci.lower, output2$test1.sp$est)
  expect_gt(output2$test1.sp$ci.upper, output2$test1.sp$est)

  expect_equal(output2$test2.sp$est, 0.775, tolerance = 1e-06)
  expect_equal(output2$test2.sp$SE, 0.02952753, tolerance = 1e-06)
  expect_equal(output2$test2.sp$ci.lower, 0.7231342, tolerance = 1e-06)
  expect_equal(output2$test2.sp$ci.upper, 0.8198996, tolerance = 1e-06)
  expect_lt(output2$test2.sp$ci.lower, output2$test2.sp$est)
  expect_gt(output2$test2.sp$ci.upper, output2$test2.sp$est)

  # Test 3: Unacceptable alpha value

  expect_error(
    conf.acc(dat, alpha = 1.1),
    "Alpha must be a value between 0 and 1, inclusive."
  )

  # Test 4: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(conf.acc(
    l1,
    "Argument must be of \"vals.1test\" or \"vals.2test\" class."
  ))

  # Test 5: Correct data for one test

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, gold)
  dat <- values.1test(recoder(df))
  output1 <- conf.acc(dat)

  expect_type(output1, "list")
  expect_type(output1$se, "list")
  expect_type(output1$sp, "list")

  expect_equal(output1$se$est, 0.75, tolerance = 1e-06)
  expect_equal(output1$se$SE, 0.02165064, tolerance = 1e-06)
  expect_equal(output1$se$ci.lower, 0.7054952, tolerance = 1e-06)
  expect_equal(output1$se$ci.upper, 0.7900933, tolerance = 1e-06)

  expect_equal(output1$sp$est, 0.725, tolerance = 1e-06)
  expect_equal(output1$sp$SE, 0.03157333, tolerance = 1e-06)
  expect_equal(output1$sp$ci.lower, 0.6596239, tolerance = 1e-06)
  expect_equal(output1$sp$ci.upper, 0.7825103, tolerance = 1e-06)
})

#-------------------------------------------------------------------------------
# CONF.PV
#-------------------------------------------------------------------------------

test_that("conf.pv function tests", {
  # Test 1: Correct values (default non-data arguments)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- conf.pv(dat)

  expect_type(output1, "list")
  expect_equal(output1$test1.ppv$est, 0.8450704, tolerance = 1e-06)
  expect_equal(output1$test1.ppv$SE, 0.01920434, tolerance = 1e-06)
  expect_equal(output1$test1.ppv$ci.lower, 0.8040245, tolerance = 1e-06)
  expect_equal(output1$test1.ppv$ci.upper, 0.8792637, tolerance = 1e-06)
  expect_lt(output1$test1.ppv$ci.lower, output1$test1.ppv$est)
  expect_gt(output1$test1.ppv$ci.upper, output1$test1.ppv$est)

  expect_equal(output1$test2.ppv$est, 0.8615385, tolerance = 1e-06)
  expect_equal(output1$test2.ppv$SE, 0.01915844, tolerance = 1e-06)
  expect_equal(output1$test2.ppv$ci.lower, 0.8200531, tolerance = 1e-06)
  expect_equal(output1$test2.ppv$ci.upper, 0.8951892, tolerance = 1e-06)
  expect_lt(output1$test2.ppv$ci.lower, output1$test2.ppv$est)
  expect_gt(output1$test2.ppv$ci.upper, output1$test2.ppv$est)

  expect_equal(output1$test1.npv$est, 0.5918367, tolerance = 1e-06)
  expect_equal(output1$test1.npv$SE, 0.03140038, tolerance = 1e-06)
  expect_equal(output1$test1.npv$ci.lower, 0.5294386, tolerance = 1e-06)
  expect_equal(output1$test1.npv$ci.upper, 0.651605, tolerance = 1e-06)
  expect_lt(output1$test1.npv$ci.lower, output1$test1.npv$est)
  expect_gt(output1$test1.npv$ci.upper, output1$test1.npv$est)

  expect_equal(output1$test2.npv$est, 0.5636364, tolerance = 1e-06)
  expect_equal(output1$test2.npv$SE, 0.02990594, tolerance = 1e-06)
  expect_equal(output1$test2.npv$ci.lower, 0.5046072, tolerance = 1e-06)
  expect_equal(output1$test2.npv$ci.upper, 0.6210392, tolerance = 1e-06)
  expect_lt(output1$test2.npv$ci.lower, output1$test2.npv$est)
  expect_gt(output1$test2.npv$ci.upper, output1$test2.npv$est)

  # Test 2: Correct values (custom non-data arguments)

  output2 <- conf.pv(dat, alpha = 0.1)

  expect_type(output2, "list")
  expect_equal(output2$test1.ppv$est, 0.8450704, tolerance = 1e-06)
  expect_equal(output2$test1.ppv$SE, 0.01920434, tolerance = 1e-06)
  expect_equal(output2$test1.ppv$ci.lower, 0.811017, tolerance = 1e-06)
  expect_equal(output2$test1.ppv$ci.upper, 0.8741704, tolerance = 1e-06)
  expect_lt(output2$test1.ppv$ci.lower, output2$test1.ppv$est)
  expect_gt(output2$test1.ppv$ci.upper, output2$test1.ppv$est)

  expect_equal(output2$test2.ppv$est, 0.8615385, tolerance = 1e-06)
  expect_equal(output2$test2.ppv$SE, 0.01915844, tolerance = 1e-06)
  expect_equal(output2$test2.ppv$ci.lower, 0.8271819, tolerance = 1e-06)
  expect_equal(output2$test2.ppv$ci.upper, 0.8902301, tolerance = 1e-06)
  expect_lt(output2$test2.ppv$ci.lower, output2$test2.ppv$est)
  expect_gt(output2$test2.ppv$ci.upper, output2$test2.ppv$est)

  expect_equal(output2$test1.npv$est, 0.5918367, tolerance = 1e-06)
  expect_equal(output2$test1.npv$SE, 0.03140038, tolerance = 1e-06)
  expect_equal(output2$test1.npv$ci.lower, 0.5395089, tolerance = 1e-06)
  expect_equal(output2$test1.npv$ci.upper, 0.6422608, tolerance = 1e-06)
  expect_lt(output2$test1.npv$ci.lower, output2$test1.npv$est)
  expect_gt(output2$test1.npv$ci.upper, output2$test1.npv$est)

  expect_equal(output2$test2.npv$est, 0.5636364, tolerance = 1e-06)
  expect_equal(output2$test2.npv$SE, 0.02990594, tolerance = 1e-06)
  expect_equal(output2$test2.npv$ci.lower, 0.5140934, tolerance = 1e-06)
  expect_equal(output2$test2.npv$ci.upper, 0.6120026, tolerance = 1e-06)
  expect_lt(output2$test2.npv$ci.lower, output2$test2.npv$est)
  expect_gt(output2$test2.npv$ci.upper, output2$test2.npv$est)

  # Test 3: Unacceptable alpha value

  expect_error(
    conf.pv(dat, alpha = 1.1),
    "Alpha must be a value between 0 and 1, inclusive."
  )

  # Test 4: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(conf.pv(
    l1,
    "Argument must be of \"vals.1test\" or \"vals.2test\" class."
  ))

  # Test 5: Correct data for one test

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, gold)
  dat <- values.1test(recoder(df))
  output1 <- conf.pv(dat)

  expect_type(output1, "list")
  expect_type(output1$PPV, "list")
  expect_type(output1$NPV, "list")

  expect_equal(output1$PPV$est, 0.8450704, tolerance = 1e-06)
  expect_equal(output1$PPV$SE, 0.000784014, tolerance = 1e-06)
  expect_equal(output1$PPV$ci.lower, 0.8140907, tolerance = 1e-06)
  expect_equal(output1$PPV$ci.upper, 0.8719779, tolerance = 1e-06)

  expect_equal(output1$NPV$est, 0.5918367, tolerance = 1e-06)
  expect_equal(output1$NPV$SE, 0.001281915, tolerance = 1e-06)
  expect_equal(output1$NPV$ci.lower, 0.5520888, tolerance = 1e-06)
  expect_equal(output1$NPV$ci.upper, 0.6305009, tolerance = 1e-06)
})

#-------------------------------------------------------------------------------
# CONF.LR
#-------------------------------------------------------------------------------

test_that("conf.lr function tests", {
  # Test 1: Correct values (default non-data arguments)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- conf.lr(dat)

  expect_type(output1, "list")
  expect_equal(output1$test1.plr$est, 2.727273, tolerance = 1e-06)
  expect_equal(output1$test1.plr$SE, 0.3228698, tolerance = 1e-06)
  expect_equal(output1$test1.plr$ci.lower, 2.17921, tolerance = 1e-06)
  expect_equal(output1$test1.plr$ci.upper, 3.46313, tolerance = 1e-06)
  expect_lt(output1$test1.plr$ci.lower, output1$test1.plr$est)
  expect_gt(output1$test1.plr$ci.upper, output1$test1.plr$est)

  expect_equal(output1$test2.plr$est, 3.111111, tolerance = 1e-06)
  expect_equal(output1$test2.plr$SE, 0.4207903, tolerance = 1e-06)
  expect_equal(output1$test2.plr$ci.lower, 2.406338, tolerance = 1e-06)
  expect_equal(output1$test2.plr$ci.upper, 4.084559, tolerance = 1e-06)
  expect_lt(output1$test2.plr$ci.lower, output1$test2.plr$est)
  expect_gt(output1$test2.plr$ci.upper, output1$test2.plr$est)

  expect_equal(output1$test1.nlr$est, 0.3448276, tolerance = 1e-06)
  expect_equal(output1$test1.nlr$SE, 0.03342614, tolerance = 1e-06)
  expect_equal(output1$test1.nlr$ci.lower, 0.2852326, tolerance = 1e-06)
  expect_equal(output1$test1.nlr$ci.upper, 0.417246, tolerance = 1e-06)
  expect_lt(output1$test1.nlr$ci.lower, output1$test1.nlr$est)
  expect_gt(output1$test1.nlr$ci.upper, output1$test1.nlr$est)

  expect_equal(output1$test2.nlr$est, 0.3870968, tolerance = 1e-06)
  expect_equal(output1$test2.nlr$SE, 0.03303944, tolerance = 1e-06)
  expect_equal(output1$test2.nlr$ci.lower, 0.3279866, tolerance = 1e-06)
  expect_equal(output1$test2.nlr$ci.upper, 0.4584056, tolerance = 1e-06)
  expect_lt(output1$test2.nlr$ci.lower, output1$test2.nlr$est)
  expect_gt(output1$test2.nlr$ci.upper, output1$test2.nlr$est)

  # Test 2: Correct values (custom non-data arguments)

  output2 <- conf.lr(dat, alpha = 0.1)

  expect_type(output2, "list")
  expect_equal(output2$test1.plr$est, 2.727273, tolerance = 1e-06)
  expect_equal(output2$test1.plr$SE, 0.3228698, tolerance = 1e-06)
  expect_equal(output2$test1.plr$ci.lower, 2.25413, tolerance = 1e-06)
  expect_equal(output2$test1.plr$ci.upper, 3.323904, tolerance = 1e-06)
  expect_lt(output2$test1.plr$ci.lower, output2$test1.plr$est)
  expect_gt(output2$test1.plr$ci.upper, output2$test1.plr$est)

  expect_equal(output2$test2.plr$est, 3.111111, tolerance = 1e-06)
  expect_equal(output2$test2.plr$SE, 0.4207903, tolerance = 1e-06)
  expect_equal(output2$test2.plr$ci.lower, 2.500768, tolerance = 1e-06)
  expect_equal(output2$test2.plr$ci.upper, 3.896612, tolerance = 1e-06)
  expect_lt(output2$test2.plr$ci.lower, output2$test2.plr$est)
  expect_gt(output2$test2.plr$ci.upper, output2$test2.plr$est)

  expect_equal(output2$test1.nlr$est, 0.3448276, tolerance = 1e-06)
  expect_equal(output2$test1.nlr$SE, 0.03342614, tolerance = 1e-06)
  expect_equal(output2$test1.nlr$ci.lower, 0.294449, tolerance = 1e-06)
  expect_equal(output2$test1.nlr$ci.upper, 0.4050507, tolerance = 1e-06)
  expect_lt(output2$test1.nlr$ci.lower, output2$test1.nlr$est)
  expect_gt(output2$test1.nlr$ci.upper, output2$test1.nlr$est)

  expect_equal(output2$test2.nlr$est, 0.3870968, tolerance = 1e-06)
  expect_equal(output2$test2.nlr$SE, 0.03303944, tolerance = 1e-06)
  expect_equal(output2$test2.nlr$ci.lower, 0.3371395, tolerance = 1e-06)
  expect_equal(output2$test2.nlr$ci.upper, 0.4464217, tolerance = 1e-06)
  expect_lt(output2$test2.nlr$ci.lower, output2$test2.nlr$est)
  expect_gt(output2$test2.nlr$ci.upper, output2$test2.nlr$est)

  # Test 3: Unacceptable alpha value

  expect_error(
    conf.lr(dat, alpha = 1.1),
    "Alpha must be a value between 0 and 1, inclusive."
  )

  # Test 4: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(conf.lr(
    l1,
    "Argument must be of \"vals.1test\" or \"vals.2test\" class."
  ))

  # Test 5: Correct data for one test

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, gold)
  dat <- values.1test(recoder(df))
  output1 <- conf.lr(dat)

  expect_type(output1, "list")
  expect_type(output1$PLR, "list")
  expect_type(output1$PLR, "list")

  expect_equal(output1$PLR$est, 2.727273, tolerance = 1e-06)
  expect_equal(output1$PLR$SE, 0.3228698, tolerance = 1e-06)
  expect_equal(output1$PLR$ci.lower, 2.17921, tolerance = 1e-06)
  expect_equal(output1$PLR$ci.upper, 3.46313, tolerance = 1e-06)

  expect_equal(output1$NLR$est, 0.3448276, tolerance = 1e-06)
  expect_equal(output1$NLR$SE, 0.03342614, tolerance = 1e-06)
  expect_equal(output1$NLR$ci.lower, 0.2852326, tolerance = 1e-06)
  expect_equal(output1$NLR$ci.upper, 0.417246, tolerance = 1e-06)
})

#-------------------------------------------------------------------------------
# OUTPUT.ACC
#-------------------------------------------------------------------------------

test_that("output.acc function tests", {
  # Test 1: Correct values (default non-data arguments)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- output.acc(dat)

  expect_equal(output1$glob.t, 31.57895, tolerance = 1e-06)
  expect_equal(output1$glob.p, 1.389053e-07, tolerance = 1e-06)
  expect_equal(output1$ind.t1, 21.05263, tolerance = 1e-06)
  expect_equal(output1$ind.t2, 10.52632, tolerance = 1e-06)
  expect_equal(output1$ind.p1, 4.468387e-06, tolerance = 1e-06)
  expect_equal(output1$ind.p2, 0.001176866, tolerance = 1e-06)

  expect_equal(output1$Mcc1, 18.05, tolerance = 1e-06)
  expect_equal(output1$Mcc2, 8.1, tolerance = 1e-06)
  expect_equal(output1$pval3a, 2.151786e-05, tolerance = 1e-06)
  expect_equal(output1$pval4a, 0.004426526, tolerance = 1e-06)
  expect_equal(output1$M1, 20, tolerance = 1e-06)
  expect_equal(output1$M2, 10, tolerance = 1e-06)
  expect_equal(output1$pval3b, 7.744216e-06, tolerance = 1e-06)
  expect_equal(output1$pval4b, 0.001565402, tolerance = 1e-06)

  # Test 2: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(output.acc(l1, "Argument must be of \"vals.2test\" class."))

  # Test 3: Non-list object without class

  v1 <- c(TP = 49, FN = 17, FP = 2, TN = 113)
  expect_error(output.acc(v1, "Argument must be of \"vals.2test\" class."))
})

#-------------------------------------------------------------------------------
# OUTPUT.PV
#-------------------------------------------------------------------------------

test_that("output.pv function tests", {
  # Test 1: Correct values (default non-data arguments)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- output.pv(dat)

  expect_equal(output1$glob.t, 28.43169, tolerance = 1e-06)
  expect_equal(output1$glob.p, 6.700962e-07, tolerance = 1e-06)
  expect_equal(output1$ind.t1, 4.059529, tolerance = 1e-06)
  expect_equal(output1$ind.t2, 6.343355, tolerance = 1e-06)
  expect_equal(output1$ind.p1, 0.04392276, tolerance = 1e-06)
  expect_equal(output1$ind.p2, 0.01178218, tolerance = 1e-06)

  # Test 2: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(output.pv(l1, "Argument must be of \"vals.2test\" class."))

  # Test 3: Non-list object without class

  v1 <- c(TP = 49, FN = 17, FP = 2, TN = 113)
  expect_error(output.pv(v1, "Argument must be of \"vals.2test\" class."))
})

#-------------------------------------------------------------------------------
# OUTPUT.LR
#-------------------------------------------------------------------------------

test_that("output.acc function tests", {
  # Test 1: Correct values (default non-data arguments)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- output.lr(dat)

  expect_equal(output1$glob.t, 24.2216, tolerance = 1e-06)
  expect_equal(output1$glob.p, 5.499788e-06, tolerance = 1e-06)
  expect_equal(output1$ind.t1, 2.013107, tolerance = 1e-06)
  expect_equal(output1$ind.t2, 2.516314, tolerance = 1e-06)
  expect_equal(output1$ind.p1, 0.04410335, tolerance = 1e-06)
  expect_equal(output1$ind.p2, 0.01185895, tolerance = 1e-06)

  # Test 2: Incorrect list object without class

  l1 <- list(s1 = 49, s0 = 17, r1 = 2, r0 = 113)
  expect_error(output.lr(l1, "Argument must be of \"vals.2test\" class."))

  # Test 3: Non-list object without class

  v1 <- list(TP = 49, FN = 17, FP = 2, TN = 113)
  expect_error(output.lr(v1, "Argument must be of \"vals.2test\" class."))
})

#-------------------------------------------------------------------------------
# MATRIXIFY
#-------------------------------------------------------------------------------

test_that("matrixify function tests", {
  # Test 1: conf.2t (not LR)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 45), rep(0, 155))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  dat <- values.2test(recoder(df))
  output1 <- matrixify(conf.acc(dat))

  expect_type(output1, "list")
  expect_equal(output1$`Test 1`[1, 1], 75.0, tolerance = 1e-06)
  expect_equal(output1$`Test 1`[2, 1], 72.5, tolerance = 1e-06)
  expect_equal(output1$`Test 2`[1, 1], 70.0, tolerance = 1e-06)
  expect_equal(output1$`Test 2`[2, 1], 77.5, tolerance = 1e-06)

  # Test 2: conf.2t (LR)

  output1 <- matrixify(conf.lr(dat))

  expect_type(output1, "list")
  expect_equal(output1$`Test 1`[1, 1], 2.7, tolerance = 1e-06)
  expect_equal(output1$`Test 1`[2, 1], 0.3, tolerance = 1e-06)
  expect_equal(output1$`Test 2`[1, 1], 3.1, tolerance = 1e-06)
  expect_equal(output1$`Test 2`[2, 1], 0.4, tolerance = 1e-06)

  # Test 3: conf.1t (not LR)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, gold)
  dat <- values.1test(recoder(df))
  output1 <- matrixify(conf.acc(dat))

  expect_type(output1, "double")
  expect_equal(output1[1, 1], 75.0, tolerance = 1e-06)
  expect_equal(output1[2, 1], 72.5, tolerance = 1e-06)

  # Test 4: conf.1t (LR)

  output1 <- matrixify(conf.lr(dat))

  expect_type(output1, "double")
  expect_equal(output1[1, 1], 2.7, tolerance = 1e-06)
  expect_equal(output1[2, 1], 0.3, tolerance = 1e-06)

  # Test 5: prev

  output1 <- matrixify(conf.prev(dat))

  expect_type(output1, "double")
})

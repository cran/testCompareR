test_that("compareR function tests", {
  # Test case 1:
  # Acceptable data frame (default non-data arguments, not significant)

  test1 <- c(rep(1, 305), rep(0, 95), rep(1, 50), rep(0, 150))
  test2 <- c(rep(1, 300), rep(0, 100), rep(1, 51), rep(0, 149))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  output1 <- compareR(df)

  expect_type(output1, "list")
  expect_type(output1$cont, "list")
  expect_type(output1$prev, "double")

  expect_type(output1$acc$accuracies, "list")
  expect_type(output1$acc$accuracies[[1]], "double")
  expect_type(output1$acc$accuracies[[2]], "double")

  expect_equal(output1$acc$glob.test.stat, 6.068316, tolerance = 1e-06)
  expect_equal(output1$acc$glob.p.value, 0.04811515, tolerance = 1e-06)
  expect_equal(output1$acc$glob.p.adj, 0.1443455, tolerance = 1e-06)
  expect_equal(output1$acc$sens.test.stat, 3.2, tolerance = 1e-06)
  expect_equal(output1$acc$sens.p.value, 0.07363827, tolerance = 1e-06)
  expect_equal(output1$acc$sens.p.adj, 0.1472765, tolerance = 1e-06)
  expect_equal(output1$acc$spec.test.stat, 0, tolerance = 1e-06)
  expect_equal(output1$acc$spec.p.value, 1, tolerance = 1e-06)
  expect_equal(output1$acc$spec.p.adj, 1, tolerance = 1e-06)

  expect_type(output1$pv$predictive.values, "list")
  expect_type(output1$pv$predictive.values[[1]], "double")
  expect_type(output1$pv$predictive.values[[2]], "double")

  expect_equal(output1$pv$glob.test.stat, 6.053805, tolerance = 1e-06)
  expect_equal(output1$pv$glob.p.value, 0.04846553, tolerance = 1e-06)
  expect_equal(output1$pv$glob.p.adj, 0.1443455, tolerance = 1e-06)
  expect_equal(output1$pv$ppv.test.stat, 2.954559, tolerance = 1e-06)
  expect_equal(output1$pv$ppv.p.value, as.double(NA), tolerance = 1e-06)
  expect_equal(output1$pv$ppv.p.adj, as.double(NA), tolerance = 1e-06)
  expect_equal(output1$pv$npv.test.stat, 5.888888, tolerance = 1e-06)
  expect_equal(output1$pv$npv.p.value, as.double(NA), tolerance = 1e-06)
  expect_equal(output1$pv$npv.p.adj, as.double(NA), tolerance = 1e-06)

  expect_type(output1$lr$likelihood.ratios, "list")
  expect_type(output1$lr$likelihood.ratios[[1]], "double")
  expect_type(output1$lr$likelihood.ratios[[2]], "double")

  expect_equal(output1$lr$glob.test.stat, 6.026798, tolerance = 1e-06)
  expect_equal(output1$lr$glob.p.value, 0.04912441, tolerance = 1e-06)
  expect_equal(output1$lr$glob.p.adj, 0.1443455, tolerance = 1e-06)
  expect_equal(output1$lr$plr.test.stat, 1.718823, tolerance = 1e-06)
  expect_equal(output1$lr$plr.p.value, as.double(NA), tolerance = 1e-06)
  expect_equal(output1$lr$plr.p.adj, as.double(NA), tolerance = 1e-06)
  expect_equal(output1$lr$nlr.test.stat, 2.426359, tolerance = 1e-06)
  expect_equal(output1$lr$nlr.p.value, as.double(NA), tolerance = 1e-06)
  expect_equal(output1$lr$nlr.p.adj, as.double(NA), tolerance = 1e-06)

  # Test case 2: Acceptable data frame (default non-data arguments, significant)

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 65), rep(0, 135))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  output1 <- compareR(df)

  expect_type(output1, "list")
  expect_type(output1$cont, "list")
  expect_type(output1$prev, "double")

  expect_type(output1$acc$accuracies, "list")
  expect_type(output1$acc$accuracies[[1]], "double")
  expect_type(output1$acc$accuracies[[2]], "double")

  expect_equal(output1$acc$glob.test.stat, 31.57895, tolerance = 1e-06)
  expect_equal(output1$acc$glob.p.value, 1.389053e-07, tolerance = 1e-06)
  expect_equal(output1$acc$glob.p.adj, 4.167158e-07, tolerance = 1e-06)
  expect_equal(output1$acc$sens.test.stat, 18.05, tolerance = 1e-06)
  expect_equal(output1$acc$sens.p.value, 2.151786e-05, tolerance = 1e-06)
  expect_equal(output1$acc$sens.p.adj, 0.0001506251, tolerance = 1e-06)
  expect_equal(output1$acc$spec.test.stat, 8.1, tolerance = 1e-06)
  expect_equal(output1$acc$spec.p.value, 0.004426526, tolerance = 1e-06)
  expect_equal(output1$acc$spec.p.adj, 0.02213263, tolerance = 1e-06)

  expect_type(output1$pv$predictive.values, "list")
  expect_type(output1$pv$predictive.values[[1]], "double")
  expect_type(output1$pv$predictive.values[[2]], "double")

  expect_equal(output1$pv$glob.test.stat, 26.92232, tolerance = 1e-06)
  expect_equal(output1$pv$glob.p.value, 1.425252e-06, tolerance = 1e-06)
  expect_equal(output1$pv$glob.p.adj, 2.850504e-06, tolerance = 1e-06)
  expect_equal(output1$pv$ppv.test.stat, 3.171214, tolerance = 1e-06)
  expect_equal(output1$pv$ppv.p.value, 0.07494674, tolerance = 1e-06)
  expect_equal(output1$pv$ppv.p.adj, 0.1498935, tolerance = 1e-06)
  expect_equal(output1$pv$npv.test.stat, 5.653882, tolerance = 1e-06)
  expect_equal(output1$pv$npv.p.value, 0.01741677, tolerance = 1e-06)
  expect_equal(output1$pv$npv.p.adj, 0.06966709, tolerance = 1e-06)

  expect_type(output1$lr$likelihood.ratios, "list")
  expect_type(output1$lr$likelihood.ratios[[1]], "double")
  expect_type(output1$lr$likelihood.ratios[[2]], "double")

  expect_equal(output1$lr$glob.test.stat, 23.37068, tolerance = 1e-06)
  expect_equal(output1$lr$glob.p.value, 8.416292e-06, tolerance = 1e-06)
  expect_equal(output1$lr$glob.p.adj, 8.416292e-06, tolerance = 1e-06)
  expect_equal(output1$lr$plr.test.stat, 1.779904, tolerance = 1e-06)
  expect_equal(output1$lr$plr.p.value, 0.07509166, tolerance = 1e-06)
  expect_equal(output1$lr$plr.p.adj, 0.1498935, tolerance = 1e-06)
  expect_equal(output1$lr$nlr.test.stat, 2.375766, tolerance = 1e-06)
  expect_equal(output1$lr$nlr.p.value, 0.01751255, tolerance = 1e-06)
  expect_equal(output1$lr$nlr.p.adj, 0.06966709, tolerance = 1e-06)

  # Test case 3: Low prevalence and n (with continuity correction)

  test1 <- c(rep(1, 6), rep(0, 2), rep(1, 14), rep(0, 76))
  test2 <- c(rep(1, 1), rep(0, 7), rep(1, 2), rep(0, 88))
  gold <- c(rep(1, 8), rep(0, 90))

  df <- data.frame(test1, test2, gold)
  output1 <- compareR(df, cc = TRUE)

  expect_type(output1, "list")
  expect_type(output1$acc$sens.p.value, "double")
  expect_type(output1$acc$spec.p.value, "double")
  expect_equal(output1$acc$glob.p.value, as.double(NA))

  # Test case 4: Low prevalence and n (without continuity correction)

  test1 <- c(rep(1, 6), rep(0, 2), rep(1, 14), rep(0, 76))
  test2 <- c(rep(1, 1), rep(0, 7), rep(1, 2), rep(0, 88))
  gold <- c(rep(1, 8), rep(0, 90))

  df <- data.frame(test1, test2, gold)
  output1 <- compareR(df, cc = FALSE)

  expect_type(output1, "list")
  expect_type(output1$acc$sens.p.value, "double")
  expect_type(output1$acc$spec.p.value, "double")
  expect_equal(output1$acc$glob.p.value, as.double(NA))

  # Test 5
  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 65), rep(0, 135))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  output1 <- compareR(df, sesp = T, ppvnpv = T, plrnlr = F)
  expect_type(output1, "list")

  # Test 6
  output1 <- compareR(df, sesp = F, ppvnpv = T, plrnlr = T)
  expect_type(output1, "list")

  # Test 7
  output1 <- compareR(df, sesp = T, ppvnpv = F, plrnlr = T)
  expect_type(output1, "list")

  # Test 8
  output1 <- compareR(df, sesp = T, ppvnpv = F, plrnlr = F)
  expect_type(output1, "list")

  # Test 9
  output1 <- compareR(df, sesp = F, ppvnpv = T, plrnlr = F)
  expect_type(output1, "list")

  # Test 10
  output1 <- compareR(df, sesp = F, ppvnpv = F, plrnlr = T)
  expect_type(output1, "list")

  # Unacceptable data frames

  df2 <- data.frame(
    col1 = as.factor(c("positive", "pos", "p", "yes", "y", "+", "1")),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2"),
    col3 = c(0, 1, 1, 0, 1, 0, 1)
  )
  expect_no_error(compareR(df2))

  df3 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", NA, "-", "0", "2"),
    col3 = c(0, 1, 1, 0, 1, 0, 1)
  )
  expect_error(compareR(df3), ".*NAs are not supported\\..*")

  df4 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "na", "-", "0", "2"),
    col3 = c(0, 1, 1, 0, 1, 0, 1)
  )
  expect_error(compareR(df4), ".*NAs are not supported\\..*")

  df5 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2"),
    col3 = c("invalid", "value", "here", "yes", "no", "yes", "+")
  )
  expect_error(compareR(df5), ".*Coding errors exist\\..*")

  df6 <- data.frame(
    col1 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col2 = c("negative", "neg", "no", "n", "-", "0", "2"),
    col3 = c("positive", "pos", "p", "yes", "y", "+", "1"),
    col4 = c("negative", "neg", "no", "n", "-", "0", "2")
  )
  expect_error(compareR(df6), ".*two or three*")

  mat <- matrix(
    sample(c(0, 1), 10, replace = TRUE),
    ncol = 2
  )
  expect_error(compareR(mat), ".*three columns\\..*")

  mat <- matrix(
    sample(3:4, 12, replace = TRUE),
    ncol = 3
  )
  expect_error(compareR(mat), ".*Coding errors exist\\..*")

  vec <- c(0, 1, 0, 1, 0)
  expect_error(
    compareR(vec),
    "Data should be provided as a data frame or matrix."
  )

  # Tests for validity of non-data arguments

  # Edge case 1: Tests have equal performance.

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  test2 <- c(rep(1, 300), rep(0, 100), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  output1 <- compareR(df)

  expect_equal(output1$acc$glob.p.value, NaN)
  expect_equal(output1$pv$glob.p.value, NaN)
  expect_equal(output1$lr$glob.p.value, NaN)
  expect_true(output1$other$equal)

  # Edge case 2: No test selected.

  test1 <- c(rep(1, 300), rep(0, 100), rep(1, 65), rep(0, 135))
  test2 <- c(rep(1, 280), rep(0, 120), rep(1, 55), rep(0, 145))
  gold <- c(rep(1, 400), rep(0, 200))

  df <- data.frame(test1, test2, gold)
  output1 <- compareR(df)

  expect_error(
    compareR(df, sesp = FALSE, ppvnpv = FALSE, plrnlr = FALSE),
    "No tests selected."
  )
})

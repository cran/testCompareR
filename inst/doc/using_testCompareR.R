## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(testCompareR)

## -----------------------------------------------------------------------------
dat <- cass

## -----------------------------------------------------------------------------
results <- compareR(dat)
results

## -----------------------------------------------------------------------------
results$acc$accuracies # returns summary tables for diagnostic accuracies

## -----------------------------------------------------------------------------
interpretR(results)

## -----------------------------------------------------------------------------
# create data frame with varied coding
df <- data.frame(
  test1 = c(" positive ", "POS ", " n ", "N ", " 1 ", "+"),
  test2 = c(" NEG ", " yes ", " negative", " Y ", "-", " 0 "),
  gold = c(0, 1, 0, 1, 2, 1)
)

# recode the dataframe
recoded <- testCompareR:::recoder(df)
recoded

## -----------------------------------------------------------------------------
# simulate data
test1 <- c(rep(1, 300), rep(0, 100), rep(1, 65), rep(0, 135))
test2 <- c(rep(1, 280), rep(0, 120), rep(1, 55), rep(0, 145))
gold <- c(rep(1, 400), rep(0, 200))

df <- data.frame(test1, test2, gold)

# test with alpha = 0.5
result <- compareR(df, alpha = 0.5)

# all results are significant
interpretR(result)

## -----------------------------------------------------------------------------
# simulate data
test1 <- c(rep(1, 300), rep(0, 100), rep(1, 65), rep(0, 135))
test2 <- c(rep(1, 280), rep(0, 120), rep(1, 55), rep(0, 145))
gold <- c(rep(1, 400), rep(0, 200))

df <- data.frame(test1, test2, gold)

# test with alpha = 0.5
result <- compareR(df, margins = TRUE)

# contingency tables have margins
result$cont

## -----------------------------------------------------------------------------
# display p.adjust.methods
p.adjust.methods

# simulate data
test1 <- c(rep(1, 300), rep(0, 100), rep(1, 65), rep(0, 135))
test2 <- c(rep(1, 280), rep(0, 120), rep(1, 55), rep(0, 145))
gold <- c(rep(1, 400), rep(0, 200))

df <- data.frame(test1, test2, gold)

# test with different multiple comparison methods
result1 <- compareR(df, multi_corr = "holm")
result2 <- compareR(df, multi_corr = "bonf")

# the more restrictive Bonferroni method returns higher adjusted p values
result1$pv$glob.p.adj < result2$pv$glob.p.adj

## -----------------------------------------------------------------------------
# simulate data
test1 <- c(rep(1, 6), rep(0, 2), rep(1, 14), rep(0, 76))
test2 <- c(rep(1, 1), rep(0, 7), rep(1, 2), rep(0, 88))
gold <- c(rep(1, 8), rep(0, 90))

df <- data.frame(test1, test2, gold)

# run compareR without continuity correction
result <- compareR(df, cc = FALSE)
result$acc

## -----------------------------------------------------------------------------
# simulate data
test1 <- c(rep(1, 317), rep(0, 83), rep(1, 68), rep(0, 132))
test2 <- c(rep(1, 281), rep(0, 119), rep(1, 51), rep(0, 149))
gold <- c(rep(1, 390), rep(0, 210))

df <- data.frame(test1, test2, gold)

# test with different multiple comparison methods
result <- compareR(df, dp = 3)

# the values in the summary tables are displayed to 3 decimal places
result$acc$accuracies

## -----------------------------------------------------------------------------
# simulate data
test1 <- c(rep(1, 317), rep(0, 83), rep(1, 68), rep(0, 132))
test2 <- c(rep(1, 281), rep(0, 119), rep(1, 51), rep(0, 149))
gold <- c(rep(1, 390), rep(0, 210))

df <- data.frame(test1, test2, gold)

# only display results for predictive values
result <- compareR(df, sesp = FALSE, plrnlr = FALSE)
result

## -----------------------------------------------------------------------------
# simulate data
test1 <- c(rep(1, 317), rep(0, 83), rep(1, 68), rep(0, 132))
test2 <- c(rep(1, 281), rep(0, 119), rep(1, 51), rep(0, 149))
gold <- c(rep(1, 390), rep(0, 210))

df <- data.frame(test1, test2, gold)

# only display results for predictive values
result <- compareR(df, test.names = c("POCT", "Lab Blood"))
result$acc$accuracies


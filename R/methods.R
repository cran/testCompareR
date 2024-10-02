#' @title Summarise a compareR object
#'
#' @description An S3 method to summarise the rather verbose output of the
#' compareR function.
#'
#' @param object An object of class compareR.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return A summary of the compareR output.
#'
#' @details
#' Method to summarise the verbose compareR output.
#'
#' @examples
#' # generate result
#' res <- compareR(cass, test1 = "exercise", test2 = "cp",
#'                 gold = "angio",
#'                 test.names = c("ExerciseStressTest", "ChestPain"))
#'
#' # run summary method
#' summary(res)
#'
#' @export

summary.compareR <- function(object, ...) {

  if (!inherits(object, "compareR")) {
    stop("The object is not of class 'compareR'")
  }

  cat("Comparison of two binary diagnostic tests using paired data\n\n")
  if(object$other$test.names[1] != "Test 1" & object$other$test.names[2] != "Test 2") {
    cat("Test 1:", object$other$test.names[1], " Test 2:", object$other$test.names[2], "\n\n")
  }
  cat("Contingency tables:\n\n")
  print(object$cont$`Gold standard vs. Test 1`)
  cat("\n")
  print(object$cont$`Gold standard vs. Test 2`)
  cat("\nSummary of comparisons:\n\n")
  printable_df <- create_table(object)
  printable_df$test <- rep(c("Test 1", "Test 2"), nrow(printable_df)/2)
  print(printable_df)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 ' ' 1")

}

#' @title Print a compareR object
#'
#' @description An S3 method to print the results verbose from the
#' compareR function.
#'
#' @param x An object of class compareR.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A printed results table of the compareR output.
#'
#' @details
#' Method to print the pertinent results of the compareR output.
#'
#' @examples
#' # generate result
#' res <- compareR(cass, test1 = "exercise", test2 = "cp",
#'                 gold = "angio",
#'                 test.names = c("ExerciseStressTest", "ChestPain"))
#'
#' # run print method
#' print(res)
#'
#' @export

print.compareR <- function(x, ...) {

  printable_df <- create_table(x)
  print(printable_df)
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 ' ' 1")

}

#' @title Plot a compareR object
#'
#' @description An S3 method to plot a simple visualisation of the results from
#' the compareR function.
#'
#' @param x An object of class compareR.
#' @param ... Arguments such as graphical parameters. Not currently in use.
#'
#' @return A visualisation of the results for diagnostic accuracies and
#' predictive values from the compareR output.
#'
#' @details
#' Method to plot the most commonly used results of the compareR output.
#'
#' @examples
#' # generate result
#' res <- compareR(cass, test1 = "exercise", test2 = "cp",
#'                 gold = "angio",
#'                 test.names = c("ExerciseStressTest", "ChestPain"))
#'
#' # run print method
#' plot(res)
#'
#' @importFrom graphics arrows axis legend points
#' @export



plot.compareR <- function(x, ...) {

  estimates <- create_table(x)[create_table(x)$metric %in%
                               c("Sensitivity", "Specificity", "PPV", "NPV"), ]

  if(nrow(estimates) == 0) {
    stop("Plot currently only supports sensitivity, specificity, PPV and NPV. Check results.")
  }

  estimates[,3:5] <- estimates[,3:5] / 100

  max_x <- nrow(estimates)/2 + 0.5
  plot(1, type = "n", xlab = "Metric", ylab = "Estimate with 95% CI",
       xlim = c(0.5, max_x), ylim = c(0, 1), xaxt = "n")

  labs <- unique(estimates$metric)
  axis(1, at = 1:(nrow(estimates)/2), labels = labs)

  jitter_values <- c(-0.15, 0.15)

  metric_positions <- c()
  for(i in 1:(nrow(estimates)/2)) {
    metric_positions <- c(metric_positions, i)
    names(metric_positions)[i] <- labs[i]
  }

  for (i in 1:nrow(estimates)) {

    x_pos <- metric_positions[estimates$metric[i]]
    x_jitter <- jitter_values[which(estimates$test[i] == c(x$other$test.names[1],
                                                           x$other$test.names[2]))]

    colour <- ifelse(estimates$test[i] == x$other$test.names[1], "darkblue",
                     "darkorange")

    points(x_pos + x_jitter, estimates$estimate[i], col = colour,
           pch = 16, cex = 1.5)
    arrows(x_pos + x_jitter, estimates$lower_ci[i],
           x_pos + x_jitter, estimates$upper_ci[i],
           angle = 90, code = 3, length = 0.1)
  }

  legend("bottomright", legend = c(x$other$test.names[1],
                                   x$other$test.names[2]),
         pch = 16, pt.cex = 1.5, col = c("darkblue", "darkorange"), bty = "n")
}

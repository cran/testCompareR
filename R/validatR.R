validatR <- function(df = NULL, test1 = NULL, test2 = NULL, gold = NULL) {

  # CHECK ARGUMENTS WHEN DF IS NULL
  if(is.null(df)) {

    if(is.null(test1) & is.null(test2) & is.null(gold)) {
      stop("No data provided.")
    }

    if(!(is.vector(test1) & is.vector(test2) & is.vector(gold))) {
      stop("When df is not provided test1, test2 and gold should be vectors of equal length.")
    }

    if(length(test1) != length(test2) | length(test2) != length(gold)) {
      stop("When df is not provided test1, test2 and gold should be vectors of equal length.")
    }

  }

  # CHECK ARGUMENTS WHEN DF IS NOT NULL
  if(!is.null(df)) {

    df <- check_coercible_to_df(df)

    if(ncol(df) < 3) {
      stop("Your data frame has less than three columns.")
    }

    if(!(is.null(test1) & is.null(test2) & is.null(gold))) {
      if (is.null(test1) | is.null(test2) | is.null(gold)) {
        stop("At least one column provided. Please provide all three columns.")
      }
      check_test_types(test1, test2, gold)
    }

  }

  # CONSTRUCT DF WHEN DF IS NOT NULL

  if(!is.null(df)) {

    if (is.null(test1) & is.null(test2) & is.null(gold)) {
      warning(
        "Using default columns. Check test 1 is first column, test 2 is second
         column and gold standard is third column."
      )
      df <- df[, 1:3]
    } else {
      df <- df[, c(test1, test2, gold)]
    }

  }

  # CONSTRUCT DF WHEN DF IS NULL

  if(is.null(df)) {

      df <- data.frame(test1, test2, gold)

  }

  return(df)

}

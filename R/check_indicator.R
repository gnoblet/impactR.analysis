#' Check that indicators contains valid values
#'
#' `check_noncrit_indicator()` check that the values for  non-critical indicators are either 0, 1 or NA. `check_crit_indicator()` check that the values for  non-critical indicators are either 1:5 or NA. Both checks that the columns are integers.
#'
#' @param df A data frame.
#' @param cols_noncrit A vector of column names for non-critical indicators (quoted).
#'
#' @return A stop statement or `TRUE`.
#'
#' @export
check_noncrit_indicator <- function(df, cols_noncrit){
  UseMethod("check_noncrit_indicator")
}

#' @export
check_noncrit_indicator.data.frame <- function(df, cols_noncrit){
  # Check for missing columns
  if_not_in_stop(df, cols_noncrit, "df")

  # Check that vectors are integers
  are_cols_integers(df, cols_noncrit)

  # Values not in set
  are_in_set(df, cols_noncrit, set = c(0, 1, NA_integer_), main_message = "All columns must only contains values 0, 1, and NA_integer_.")


  return(TRUE)
}


#' @export
#'
#' @rdname check_noncrit_indicator
#'
#' @param cols_crit A vector of column names for critical indicators (quoted).
#'
check_crit_indicator <- function(df, cols_crit){
  UseMethod("check_crit_indicator")
}

#' @export
check_crit_indicator.data.frame <- function(df, cols_crit){
  # Check for missing columns
  if_not_in_stop(df, cols_crit, "df")

  # Check that vectors are integers
  are_cols_integers(df, cols_crit)

  # Values not in set
  are_in_set(df, cols_crit, set = c(1:5, NA_integer_), main_message = "All columns must only contains values 1 to 5 or NA_integer_.")


  return(TRUE)
}

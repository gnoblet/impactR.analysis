#' @title Stop statement "If not in colnames"
#'
#' @param .tbl A tibble
#' @param cols A vector of column names (quoted)
#' @param df Provide the tibble name as a character string
#' @param arg Default to NULL.
#'
#' @return A stop statement
if_not_in_stop <- function(.tbl, cols, df, arg = NULL){

  missing_cols <- impactR.utils::subvec_not_in(cols, colnames(.tbl))

  if (is.null(arg)) {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns are missing in `{df}`:")
    } else {
      msg <- glue::glue("The following column is missing in `{df}`:")
    }
  }
  else {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns from `{arg}` are missing in `{df}`:")
    } else {
      msg <- glue::glue("The following column from `{arg}` is missing in `{df}`:")
    }
  }
  if (length(missing_cols) >= 1) {
    rlang::abort(
      c("Missing columns",
        "*" =
          paste(
            msg,
            paste(
              missing_cols,
              collapse = ", ")
          )
      )
    )
  }
}

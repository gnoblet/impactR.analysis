#' @title Stop statement "If not in colnames"
#'
#' @param df A dataframe.
#' @param cols A vector of column names (quoted).
#' @param df_name Provide the tibble name as a character string.
#' @param arg Default to NULL.
#'
#' @return A stop statement.
#'
if_not_in_stop <- function(df, cols, df_name, arg = NULL){

  missing_cols <- impactR.utils::subvec_not_in(cols, colnames(df))

  if (is.null(arg)) {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns are missing in `{df_name}`:")
    } else {
      msg <- glue::glue("The following column is missing in `{df_name}`:")
    }
  }
  else {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns from `{arg}` are missing in `{df_name}`:")
    } else {
      msg <- glue::glue("The following column from `{arg}` is missing in `{df_name}`:")
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


# Just a helper to not copy-paste in each svy_ function for grouping
add_group_key <- function(df, group, interact_key, interact_key_sep, before){

  # Add group key
  df[["group_key"]] <- group_key

  # Add group key values
  df[["group_key_value"]] <- do.call(paste, c(df[group], sep = group_key_sep))

  # Place group_key in front
  df <- dplyr::relocate(df, "group_key_value", .before = dplyr::all_of(before))
  df <- dplyr::relocate(df, "group_key", .before = "group_key_value")

  return(df)
}

# Just a helper to not copy-paste in each svy_ function for interacting
add_interact_key <- function(df, interact, interact_key, interact_key_sep, before){

  # Add group key
  df[["interact_key"]] <- interact_key

  # Add group key values
  df[["interact_key_value"]] <- do.call(paste, c(df[interact], sep = interact_key_sep))

  # Place group_key in front
  df <- dplyr::relocate(df, "interact_key_value", .before = dplyr::all_of(before))
  df <- dplyr::relocate(df, "interact_key", .before = "interact_key_value")


  return(df)
}



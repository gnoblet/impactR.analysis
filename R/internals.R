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
      c("Missing columns.",
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
add_group_key <- function(df, group,  group_key, group_key_sep, before){

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


# Check if cols are integers
are_cols_integers <- function(df, cols){

  # Check that vectors are integers (to enforce values and check consistancy)
  is_integer <- sapply(df[cols], typeof) %in% "integer"

  if(!all(is_integer)) rlang::abort(c(
    "Non-critical indicators must be of type 'integer'.",
    "i" = glue::glue("This or these columns are not of type 'integer': ", paste(cols[!is_integer], collapse = ", "), ".")
  ))
}


# Check if value are not in set
are_in_set <- function(df, cols, set, main_message){

  # Values not in set
  values <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(cols)
    ),
    \(x) {
      all(x %in% set)
    }
  )

  cols <- cols[!values]

  if (any(!values)) {
    rlang::abort(c(
      glue::glue(main_message),
      "i" = glue::glue(
        "The following columns have values out of the above set: ",
        glue::glue_collapse(cols, sep = ",", last = " and "), "."
      )
    ))
  }

  return(TRUE)
}



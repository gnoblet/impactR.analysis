# @title Stop statement "If not in colnames"
#
# @param df A dataframe.
# @param cols A vector of column names (quoted).
# @param df_name Provide the tibble name as a character string.
# @param arg Default to NULL.
#
# @return A stop statement.
#
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
are_cols_type <- function(df, cols, type){

  # Check that vectors are types (to enforce values and check consistency)
  is_type <- sapply(df[cols], typeof) %in% type

  if(!all(is_type)) rlang::abort(c(
    paste0("Non-critical indicators must be of type ", type),
    "i" = glue::glue("This or these columns are not of type 'integer': ", paste(cols[!is_type], collapse = ", "), ".")
  ))
}




# Check if cols are integers
are_cols_integers <- function(df, cols){

  # Check that vectors are integers (to enforce values and check consistency)
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


# Adds the analysis key to the results table from a svy_*() function
add_analysis_key <- function(results, group_key_name = "group_key", group_key_value_name = "group_key_value", group_key_sep = " -/- ", var_name = "var", var_value_name = "var_value", var_key_sep = " -/- ",  overall_sep = " @/@ ", main_sep =  " -/- ", var_to_value_sep = " %/% ") {


  # Extract group values
  x <- stringr::str_split(results[[group_key_name]], group_key_sep)
  y <- stringr::str_split(results[[group_key_value_name]], group_key_sep)

  to_add_group <- purrr::map2(
    x,
    y,
    function(x, y) {
      paste(x, y, sep = var_to_value_sep)
    })

  to_add_group <- purrr::map_chr(to_add_group, stringr::str_c, collapse = main_sep)


  # Extract var values
  x <- stringr::str_split(results[[var_name]], var_key_sep)
  y <- stringr::str_split(results[[var_value_name]], var_key_sep)

  to_add_var<- purrr::map2(
    x,
    y,
    function(x, y) {
      paste(x, y, sep = var_to_value_sep)
    })

  to_add_var <- purrr::map_chr(to_add_var, stringr::str_c, collapse = main_sep)


  # Final mutate
  results <- dplyr::mutate(
    results,
    analysis_key = paste0(
      !!rlang::sym("stat_type"),
      overall_sep,
      to_add_var,
      overall_sep,
      to_add_group
    )
  )

  return(results)
}

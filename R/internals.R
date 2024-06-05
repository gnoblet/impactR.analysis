
# Analysis key ------------------------------------------------------------


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





# Stop statement ----------------------------------------------------------


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


# Stop statement values are not numeric
are_cols_numeric <- function(df, cols){

  #------ Check for missing columns
  if_not_in_stop(df, cols, "df")

  classes <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(cols)
    ),
    is.numeric
  )

  cols <- cols[!classes]

  if (!all(classes)) {
    rlang::abort(c(
      "All columns must be numeric.",
      "i" = glue::glue(
        "The following columns are not numeric. Please check.\n",
        glue::glue_collapse(cols, sep = "\n")
      )
    ))
  }

  return(TRUE)
}




# Stop statement values are not in range
are_values_in_range <- function(df, cols, lower = 0, upper = 7){

  #------ Only use on numeric columns
  are_cols_numeric(df, cols)

  ranges <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(cols)
    ),
    \(x) {
      sum(x < lower | x > upper, na.rm = TRUE) >= 1
    }
  )

  cols <- cols[ranges]

  if (all(ranges)) {
    rlang::abort(c(
      glue::glue("All columns must be between {lower} and {upper}."),
      "i" = glue::glue(
        "The following columns have values outside the range Please check.\n",
        glue::glue_collapse(cols, sep = "\n")
      )
    ))
  }

  return(TRUE)
}


# Stop statement values are not in set
are_values_in_set <- function(df, cols, set, main_message = "All columns must be in the following set: "){

  #------ Check for missing columns
  if_not_in_stop(df, cols, "df")

  #------ Values not in set
  values_lgl <- purrr::map_lgl(
    dplyr::select(
      df,
      dplyr::all_of(cols)
    ),
    \(x) {
      !all(stats::na.omit(unique(x)) %in% set)
    }
  )

  if (any(values_lgl)) {

    cols <- cols[values_lgl]
    values_chr <- names(values_lgl)

    # Get values not in set
    df_cols <- dplyr::select(df, dplyr::all_of(cols))
    values_chr <- purrr::map(df_cols, \(x) {
      x <- unique(x)
      x[!is.na(x) & !(x %in% set)]
    })

    values_chr <- purrr::imap_chr(values_chr, \(x, idx) {
      glue::glue("{idx}: {glue::glue_collapse(x, sep = ', ', last = ' and ')}")
    })

    rlang::abort(c(
      glue::glue(main_message, glue::glue_collapse(set, sep = ", ")),
      "i" = glue::glue(
        "The following columns have values out of the set Please check.\n",
        glue::glue_collapse(cols, sep = "\n")
      ),
      "x" = glue::glue("The values out of the set are:\n", glue::glue_collapse(values_chr, sep = "\n"))
    ))
  }

  return(TRUE)
}



# Subvec in
subvec_in <- function(vector, set) {
  vector[vector %in% set]
}

# Subvec not in
subvec_not_in <- function(vector, set) {
  vector[!(vector %in% set)]
}



# Stop statement "If not in colnames" with colnames
if_not_in_stop <- function(df, cols, df_name, arg = NULL) {

  missing_cols <- subvec_not_in(cols, colnames(df))

  if (is.null(arg)) {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns are missing in `{df_name}`: ")
    } else {
      msg <- glue::glue("The following column is missing in `{df_name}`: ")
    }
  } else {
    if (length(missing_cols) >= 2) {
      msg <- glue::glue("The following columns from `{arg}` are missing in `{df_name}`: ")
    } else {
      msg <- glue::glue("The following column from `{arg}` is missing in `{df_name}`: ")
    }
  }
  if (length(missing_cols) >= 1) {
    rlang::abort(
      c("Missing columns",
        "*" =
          glue::glue(
            msg,
            glue::glue_collapse(
              missing_cols,
              sep = ", ",
              last = ", and "
            )
          )
      )
    )
  }
}


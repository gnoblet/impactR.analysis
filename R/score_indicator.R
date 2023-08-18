#' Calculate the score for the non-critical indicators component
#'
#' `score_noncrit_indicator()` calculates the score for the non-critical indicators. `score_crit_indicator()` calculates the score for the critical indicators.
#'
#' @param df A data frame.
#' @param ... The variables for the component of non-critical indicators (quoted or not).
#' @param new_colname The new column name. It will override it in the dataframe if it already exists.
#'
#' @return A data frame with a new column 'score_noncrit' or 'score_crit'.
#'
#' @export
score_noncrit_indicator <- function(df, ..., new_colname = "score_noncrit"){
  UseMethod("score_noncrit_indicator")
}

#' @export
score_noncrit_indicator.data.frame <- function(df, ...){

  quoted_cols <- purrr::map_chr(rlang::enquos(...), rlang::as_name)

  # Check if these columns exists
  if_not_in_stop(df, quoted_cols, "df", arg = "...")

  # At least two indicators
  if (length(quoted_cols) < 2) rlang::abort(c(
    "Less than two non-critical indiacators provided.",
    "i" = "Please provide at least two non-critical indicators. A non-critical indicator by itself cannot lead to any severy level above 1.")
  )

  # Check values
  check_noncrit_indicator(df, quoted_cols)

}

#' @export
#'
#' @rdname score_noncrit_indicator
#'
#' @param crit_4plus Vector of critical variables giving up to a severity level of 4+ (quoted).
#' @param crit_4 Vector of critical variables giving up to a severity level of 4 (quoted).
#' @param crit_3 Vector of critical variables giving up to a severity level of 3 (quoted).
#'
#' @details A severity of 4+ should be coded as 5 in the dataset.
#'
score_crit_indicator <- function(df, crit_4plus = NULL, crit_4 = NULL, crit_3 = NULL, new_colname = "score_crit"){
  UseMethod("score_crit_indicator")
}

#' @export
score_crit_indicator.data.frame <- function(df, crit_4plus = NULL, crit_4 = NULL, crit_3 = NULL, new_colname = "score_crit"){

  # non null column names
  crit <- c(crit_4plus, crit_4, crit_3)

  # Check if these columns exists
  if_not_in_stop(df, crit_4plus, "df", arg = "crit_4plus")
  if_not_in_stop(df, crit_4, "df", arg = "crit_4")
  if_not_in_stop(df, crit_3, "df", arg = "crit_3")

  # Check if there is any duplication
  if (length(unique(crit)) < length(crit)) rlang::abort("There are duplicated variables across 'crit_4plus', 'crit_4', and 'crit_4'. Please check.")

  # Check values
  check_crit_indicator(df, crit)

  # Prepare dummy for NULL variables to be reused
  crit_null <- is.null(crit_4plus) & is.null(crit_4) & is.null(crit_3)
  crit_4plus_null <- is.null(crit_4plus)
  crit_4andabove_null <- is.null(crit_4plus) & is.null(crit_4plus)
  crit_4_null <- is.null(crit_4)
  crit_3_null <- is.null(crit_3)
  crit_3and4_null <- is.null(crit_3) & is.null(crit_4)
  crit_3and4_plus_null <-  is.null(crit_3) & is.null(crit_4plus)
  crit_none_null <- !is.null(crit_4plus) & !is.null(crit_4) & !is.null(crit_3)
  # crit_4plus_only_null <- is.null(crit_4plus) & !is.null()

  # level_non_null <- purrr::keep(list(crit_4plus = crit_4plus, crit_4 = crit_4, crit_3 = crit_3), ~ !is.null(.x))



  # Are all crit NULL?
  if (crit_null) rlang::abort("There is no critical indicator provided (all params are NULL). Please provide at least one.")

  # Get max level and throw error is some values per level are out of the range
  max_level <- if (crit_4andabove_null){

    are_in_set(df, crit_3, set = c(1:3, NA_integer_), main_message = "Severity level above 3 exists for 'crit_3'.")
    list(var = crit_3, level = 3)

  } else if (crit_4plus_null){

    are_in_set(df, crit_3, set = c(1:3, NA_integer_), main_message = "Severity level above 3 exists for 'crit_3'.")
    are_in_set(df, crit_4, set = c(1:4, NA_integer_), main_message = "Severity level above 4 exists for 'crit_4'.")
    list(var = crit_4, level = 4)

  } else {

    are_in_set(df, crit_3, set = c(1:3, NA_integer_), main_message = "Severity level above 3 exists for 'crit_3'.")
    are_in_set(df, crit_4, set = c(1:4, NA_integer_), main_message = "Severity level above 4 exists for 'crit_4'.")
    list(var = crit_4plus, level = 5)

  }

  # Get the max value per severity level
  df <- dplyr::rowwise(df)
  if (!crit_4plus_null) df <- dplyr::mutate(df, "max_crit_4plus" := pmax(!!!rlang::syms(crit_4plus), na.rm = TRUE)) else df <- dplyr::mutate(df, "max_crit_4plus" := NA_integer_)
  if (!crit_4_null) df <- dplyr::mutate(df, "max_crit_4" := pmax(!!!rlang::syms(crit_4), na.rm = TRUE)) else df <- dplyr::mutate(df, "max_crit_4" := NA_integer_)
  if (!crit_3_null) df <- dplyr::mutate(df, "max_crit_3" := pmax(!!!rlang::syms(crit_3), na.rm = TRUE)) else df <- dplyr::mutate(df, "max_crit_3" := NA_integer_)
  df <- dplyr::ungroup(df)

  # Big case when:
  df <- dplyr::mutate(
    df,
    !!rlang::sym(new_colname) := dplyr::case_when(

      #------ STRAIGHT FORWARD

      #--- NONE of the submitted columns is NA, easy pick
      dplyr::if_all(dplyr::all_of(crit), \(x) !is.na(x)) ~ pmax(!!!rlang::syms(crit), na.rm = FALSE),

      #--- IF max level is 4+ and any scores 4+
      # THEN 4+
      max_level$level == 5 & max_crit_4plus == 5 ~ 5,

      #--- IF max level is 4 and any scores 4
      # THEN 4
      max_level$level == 4 & max_crit_4 == 4 ~ 4,

      #--- IF max level is 3 any score 3,
      # THEN 3
      max_level$level == 3 & max_crit_3 == 3 ~ 3,


      #------ IF higher levels are NULL and any of the higher level is NA

      #--- IF the max level is 4+
      # AND any of the severity up to 4+ critical indicators is NA
      # AND none of the others is 4+,
      # THEN NA
      max_level$level == 5 & dplyr::if_any(dplyr::all_of(crit_4plus), \(x) is.na(x)) & max_crit_4plus < 5 ~ NA_integer_,

      #--- IF the max level is 4
      # AND any of the severity up to 4 critical indicators is missing
      # AND none of the others is 4,
      # THEN NA

      max_level$level == 4 & dplyr::if_any(dplyr::all_of(crit_4), \(x) is.na(x)) & max_crit_4 < 4 ~ NA_integer_,

      #--- IF the max level is 3
      # AND any of the severity up to 3 critical indicators is missing
      # AND none of the others is 3,
      # THEN NA
      max_level$level == 3 & dplyr::if_any(dplyr::all_of(crit_3), \(x) is.na(x)) & max_crit_4 < 3 ~ NA_integer_,

      #----- If the max level is 4+
      # AND level 4 indicators exists
      # AND any of the the severity up to 4 critical indiactors is missing
      # AND none of the remaining scores 4
      # THEN NA_integer_
      max_level$level == 5 & !crit_4_null & max_crit_4plus < 4 & max_crit_4 < 4 & dplyr::if_any(dplyr::all_of(crit_4), \(x) is.na(x)) ~ NA_integer_,

      #----- If the max level is 4
      # AND level 4 indicators exists
      # AND any of the the severity up to 4 critical indiactors is missing
      # AND none of them scores 4
      # THEN NA_integer_
      max_level$level == 4 & !crit_3_null & max_crit_4 < 3 & max_crit_4 < 3 & dplyr::if_any(dplyr::all_of(crit_3), \(x) is.na(x)) ~ NA_integer_,


      #------ 4+ ONLNY is null
      #--- IF 4 is NA
      # !crit_4_null &

      #------ None is NULL

      #--- IF the max level is 4+
      # AND any of the severity up to 4+ critical indicators is NA
      # AND none of the others is 4+,
      # THEN NA
      max_level$level == 5 & dplyr::if_any(dplyr::all_of(crit_4plus), \(x) is.na(x)) & max_crit_4plus < 5 ~ NA_integer_,

      #--- IF the max level is 4
      # AND any of the severity up to 4 critical indicators is missing
      # AND none of the others is 4,
      # THEN NA

      max_level$level == 4 & dplyr::if_any(dplyr::all_of(crit_4), \(x) is.na(x)) & max_crit_4 < 4 ~ NA_integer_,

      #--- IF the max level is 3
      # AND any of the severity up to 3 critical indicators is missing
      # AND none of the others is 3,
      # THEN NA
      max_level$level == 3 & dplyr::if_any(dplyr::all_of(crit_3), \(x) is.na(x)) & max_crit_4 < 3 ~ NA_integer_,

      #--- IF

      # If both 3 and 4 are NULL, take the 4+ indicator
      crit_3and4_null ~ max_crit_4plus,
      # If both 3 and 4+ are NULL, take the 4 indicator
      crit_3and4_plus_null ~ max_crit_4,
      # If both 4 and 4+ are NULL, take the 3 indicator
      crit_4andabove_null ~ max_crit_3,
      # crit_4plus_null ~ max(max_crit_3, max_crit_4, na.rm = TRUE),
      .default = 999
    )
  )

  # The workflow should be as follows:
  # - Check values according to the highest possibles values
  # - Check if all are not null

  return(df)

}


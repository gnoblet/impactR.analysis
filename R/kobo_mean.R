#' Get the mean for a numeric var
#'
#' @param design A srvyr::design object.
#' @param vars Variables to calculate mean from.
#' @param survey The survey sheet from Kobo (with column "type" split). See [impactR.kobo::split_survey()].
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve var's label from the survey sheet? Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @inheritParams svy_mean
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A character vector of select_one questions.
#'
#' @export
kobo_mean <- function(design, vars, survey, group = NULL, group_key_sep = " ~/~ ", label_survey = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95){


  #------ Gather arguments

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  #------ Checks

  # Check if vars are in design
  if_not_in_stop(design, vars, "design")


  # Check survey columns
  if_not_in_stop(survey, c("type", "name"), "survey")

  # Checks already in svy_mean()
  # - col and group existence, group and col identical
  # - level below 0.9 warning

  # Checks if 1. there are no numeric variables, and 2. that vars are in survey
  suppressWarnings(
    num_vars <-  c(
      impactR.kobo::get_survey_decimal(survey),
      impactR.kobo::get_survey_calculate(survey),
      impactR.kobo::get_survey_integer(survey)
    )
  )

  # 1.
  if (length(num_vars) == 0) {

    rlang::warn("There are no vars of types decimal, calculate or integer. An empty tibble is returned.")
    return (dplyr::tibble())

  }

  # 2.
  if (any(!(vars %in% num_vars))) {

    vars_not_num_vars <- vars[!(vars %in% num_vars)]

    rlang::abort(
      c("Variable is not a numeric variable in survey (calculate, decimal, integer).",
        "i" = glue::glue("You may check that column ", glue::glue_collapse(vars_not_num_vars, sep = ", ", last = " and "), " is/are either a 'calculate', 'decimal' or 'integer' in 'survey'. Maybe verify that the survey sheet is the right and most updated version.")
      )
    )

  }

  # Calculate proportion
  median <- svy_mean(design, vars, group =  group, vartype = vartype, level = level, na_rm = na_rm)

  # Type of analysis is select_multiple
  median[["analysis"]] <- "numeric"

  if (label_survey) {

    label <- impactR.kobo::get_survey_labels(survey, !!!vars, output_df = TRUE)
    label <- dplyr::rename(label, "var_label" := !!rlang::sym("label"))
    median <- dplyr::left_join(median, label, by = c("var" = "name"))
    # proportion <- dplyr::mutate(proportion, "var_label" = ifelse(length(label) == 0, NA_character_, label))

  }

  return(median)

}

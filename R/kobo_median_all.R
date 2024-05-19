#' Get all numeric variables's medians
#'
#' @param design A srvyr::design object.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param label_survey Boolean. Retrieve questions label from the survey sheet? Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @family functions for analyzing from Kobo tool
#'
#' @inheritParams svy_median
#'
#' @return A character vector of select_one questions.
#'
#' @export
kobo_median_all <- function(design, survey, group = NULL, group_key_sep = " -/- ", label_survey = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95){

  suppressWarnings(num_vars <- c(
    impactR.kobo::get_survey_decimal(survey),
    impactR.kobo::get_survey_calculate(survey),
    impactR.kobo::get_survey_integer(survey)
    )
  )

  # if no variable to analyze
  if (length(num_vars) == 0) {

    rlang::warn("There are no vars of types decimal, calculate or integer. An empty tibble is returned.")
    return (dplyr::tibble())

  }

  # select_ones that are not grouping columns
  num_vars <- num_vars[!(num_vars %in% group)]

  medians <- kobo_median(design = design, vars = num_vars, survey = survey, group =  group, label_survey = label_survey, na_rm = na_rm, vartype = vartype, level = level)

  return(medians)

}

#' Get all select_multiples proportions
#'
#' @param design A srvyr::design object.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param choices The choices sheet from Kobo.
#' @param choices_sep Select multiples choices separator.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve questions label from the survey sheet? Default to TRUE.
#' @param label_choices Boolean. Retrieve choices label from the choices sheet? Default to TRUE.
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
proportion_all_select_multiple <- function(design, survey, choices = NULL, group = NULL, choices_sep = "_", label_survey = TRUE, label_choices = TRUE, na_rm = TRUE, stat_name = "prop", vartype = "ci", level = 0.95){

  # Check
  if_not_in_stop(survey, c("type", "name"), "survey")
  if (!is.null(choices)) if_not_in_stop(choices, c("label", "name"), "choices")

  select_multiples <- impactR.kobo::get_survey_select_multiple(survey)

  # select_multiples that exists in design
  select_multiples <- select_multiples[select_multiples %in% colnames(design)]

  # select_multiples that are not grouping columns
  select_multiples <- select_multiples[!(select_multiples %in% group)]

  proportions <- purrr::map(select_multiples, \(x) proportion_select_multiple(design, col = {{ x }}, survey = survey, choices = choices, group =  group, label_survey = label_survey, label_choices = label_choices, na_rm = na_rm, vartype = vartype, level = level))

  proportions <- purrr::set_names(proportions, select_multiples)


  return(proportions)

}

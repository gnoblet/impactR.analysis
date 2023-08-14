#' Get all select_one proportions
#'
#' @param design A srvyr::design object.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param choices The choices sheet from Kobo. If not NULL, the function tries to retrieve labels.
#' @param label_survey Boolean. Retrieve questions label from the survey sheet? Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @family functions for analyzing from Kobo tool
#'
#' @inheritParams svy_prop
#'
#' @return A character vector of select_one questions.
#'
#' @export
proportion_all_select_one <- function(design, survey, choices = NULL, group = NULL, group_key_sep = "*", label_survey = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95){

  # Check
  if_not_in_stop(survey, c("type", "name"), "survey")
  if (!is.null(choices)) if_not_in_stop(choices, c("label", "name"), "choices")

  select_ones <- impactR.kobo::get_survey_select_one(survey)

  # select_ones that exists in design
  select_ones <- select_ones[select_ones %in% colnames(design)]

  # select_ones that are not grouping columns
  select_ones <- select_ones[!(select_ones %in% group)]

  proportions <- purrr::map(select_ones, \(x) {
    prop <- proportion_select_one(design = design, col = {{ x }}, survey = survey, choices = choices, group =  group, label_survey = label_survey, na_rm = na_rm, vartype = vartype, level = level)

    prop <- dplyr::mutate(prop, "var_value" := as.character(!!rlang::sym("var_value")))
    })

  proportions <- purrr::list_rbind(proportions)

  return(proportions)

}

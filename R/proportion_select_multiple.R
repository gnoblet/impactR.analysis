#' Get the proportion for a select_multiple
#'
#' @param design A srvyr::design object.
#' @param col A column to calculate proportion from.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param choices The choices sheet from Kobo.
#' @param choices_sep Select multiples choices separator.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve questions label from the survey sheet? Default to TRUE.
#' @param label_choices Boolean. Retrieve choices label from the choices sheet? Default to TRUE.
#'
#' @inheritParams svy_mean
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A character vector of select_one questions.
#'
#' @export
proportion_select_multiple <- function(design, col, survey, choices, choices_sep = "_", group = NULL, label_survey = TRUE, label_choices = TRUE, na_rm = TRUE, stat_name = "prop", vartype = "ci", level = 0.95){

  # Get column name
  col_name <- rlang::as_name(rlang::enquo(col))

  # Check
  if_not_in_stop(survey, c("type", "name"), "survey")
  if (!is.null(choices)) if_not_in_stop(choices, c("label", "name"), "choices")

  # select_multiples <- impactR.kobo::get_survey_select_multiple(survey)

  # select_multiples that exists in design
  # select_multiples <- select_multiples[select_multiples %in% colnames(design)]

  # select_multiples that are not grouping columns
  # select_multiples <- select_multiples[!(select_multiples %in% group)]

  select_multiple_child <- impactR.kobo::get_survey_choices(survey, choices, {{ col }}, sep = choices_sep)

  labels <- impactR.kobo::get_survey_choices(survey, choices, {{ col }}, sep = choices_sep, label = TRUE)

  proportions <- purrr::map2(select_multiple_child, labels[["name"]], \(x,y) {

    # Get proportion per choice
    proportion <- svy_mean(design, col = !!rlang::sym(x), group = group, stat_name = "prop", vartype = vartype, na_rm = na_rm, level = level)

    # Remove the overall name and replace by child choices only
    proportion <- impactR.utils::deselect(proportion, "name")
    dplyr::mutate(proportion, "value" = {{ y }}, .before = "prop")

  })

  proportions <- dplyr::bind_rows(proportions)

  proportions <- dplyr::mutate(proportions, "name" := col_name, .before = "value")

  if (label_survey) {

    label <- impactR.kobo::get_survey_labels(survey, !!rlang::sym(col_name), output_df = FALSE)
    proportions <- dplyr::mutate(proportions, "label_name" = ifelse(length(label) == 0, NA_character_, label))

  }

  if (label_choices) {

    labels <- dplyr::rename(labels, "label_value" = !!rlang::sym("label"))

    labels <- dplyr::mutate(labels, "name" = as.character(!!rlang::sym("name")))

    proportions <- dplyr::mutate(proportions, "value" = as.character(!!rlang::sym("value")))
    proportions <- dplyr::left_join(proportions, labels, by = c("value" = "name"))

  }

  return(proportions)

}

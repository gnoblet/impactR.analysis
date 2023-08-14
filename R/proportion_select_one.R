#' Get the proportion for a select_one
#'
#' @param design A srvyr::design object.
#' @param col A column to calculate proportion from.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param choices The choices sheet from Kobo. If not NULL, the function tries to retrieve labels.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve questions label from the survey sheet? Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @inheritParams svy_prop
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A character vector of select_one questions.
#'
#' @export
proportion_select_one <- function(design, col, survey, choices = NULL, group = NULL, group_key_sep = "*", label_survey = TRUE, na_rm = TRUE, stat_name = "prop", vartype = "ci", level = 0.95){


  #------ Gather arguments

  # Get col name
  col_name <- rlang::as_name(rlang::enquo(col))

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  #------ Checks
  # Checks already in svy_prop()
  # - col and group existence, group and col identical
  # - level below 0.9 warning

  # TO DO:
  # - Add checks on survey if need, see with impactR.kobo
  # Should it check if the variable is a select one? i would say no, intended use, just labels will be empty

  # Check survey and choices columns
  if_not_in_stop(survey, c("type", "name"), "survey")
  if (!is.null(choices)) if_not_in_stop(choices, c("label", "name"), "choices")

  proportion <- svy_prop(design, {{ col }}, group =  group, vartype = vartype, level = level, stat_name = stat_name, na_rm = na_rm)

  if (label_survey) {

    label <- impactR.kobo::get_survey_labels(survey, !!rlang::sym(col_name), output_df = FALSE)
    proportion <- dplyr::mutate(proportion, "var_label" = ifelse(length(label) == 0, NA_character_, label))

  }

  if (!is.null(choices)) {

      labels <- impactR.kobo::get_survey_choices(survey, choices, {{ col }}, label = TRUE)
      labels <- dplyr::rename(labels, "var_value_label" = !!rlang::sym("label"))

      labels <- dplyr::mutate(labels, "name" = as.character(!!rlang::sym("name")))

      proportion <- dplyr::mutate(proportion, "var_value" = as.character(!!rlang::sym("var_value")))
      proportion <- dplyr::left_join(proportion, labels, by = c("var_value" = "name"))

  }

  return(proportion)

}

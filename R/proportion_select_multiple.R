#' Get the proportion for a select_multiple
#'
#' @param design A srvyr::design object.
#' @param var A variable to calculate proportion from.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param choices The choices sheet from Kobo.
#' @param choices_sep Select multiples choices separator.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve var's label from the survey sheet? Default to TRUE.
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
proportion_select_multiple <- function(design, var, survey, choices, choices_sep = "_", group = NULL, group_key_sep = "*", label_survey = TRUE, label_choices = TRUE, na_rm = TRUE, stat_name = "prop", vartype = "ci", level = 0.95){

  # Get column name
  var_name <- rlang::as_name(rlang::enquo(var))

  # Check
  if_not_in_stop(survey, c("type", "name"), "survey")
  if (!is.null(choices)) if_not_in_stop(choices, c("label", "name"), "choices")

  select_multiples <- impactR.kobo::get_survey_select_multiple(survey)

  # select_multiples that exists in design
  if (!(var_name %in% select_multiples)) rlang::abort(
    c("Variable is not a select_multiple in survey.",
      "i" = glue::glue("You may check that column '{var_name}' is a `select_multiple` question. Maybe verify that the survey sheet is the right and most updated version.")
    ))

  # select_multiples that are a grouping column
  if (var_name %in% group) rlang::abort("Grouping columns in `group` should be different than `var`.")

  # get childs
  select_multiple_child <- impactR.kobo::get_survey_choices(survey, choices, !!rlang::sym(var_name), sep = choices_sep)

  # keep only childs that are in the datasets
  select_multiple_child_in_design <- impactR.utils::subvec_in(select_multiple_child, colnames(design))

  # STOP if there is not child column found in the dataset
  if (length(select_multiple_child_in_design) == 0) rlang::abort(
    c("No child column.",
      "*" = glue::glue("There is no child column found for column '{var_name}' in `design`. The 1/0 child columns are needed for calculation."),
      "i" = glue::glue("You may check that column '{var_name}' is a `select_multiple` question. You may also check that the provided separator for choices is the right one (param 'choices_sep' default to '_'). Finally, you can verify that the survey sheet is the right and most updated version.")
    ))

  # if SOME child columns are in survey but not in the dataset, warn that they were discarded.
  select_multiple_child_not_in_design <- impactR.utils::subvec_not_in(select_multiple_child, select_multiple_child_in_design)
  if (length(select_multiple_child_not_in_design) > 0) rlang::warn(glue::glue(paste0("The following child columns of '{var_name}' does not exist in `design`, there are removed for the proportion calculation: ", paste(
    select_multiple_child_not_in_design,
    collapse = ", "))))

  # Get labels for later and then unite the name and column to grap the childs and filter out the non-existent ones
  # That warning + going through behavior could actually be changed if needed
  labels <- impactR.kobo::get_survey_choices(survey, choices, !!rlang::sym(var_name), sep = choices_sep, label = TRUE)
  labels <- tidyr::unite(labels, "child", "col", "name", sep = choices_sep, remove = FALSE)
  labels <- dplyr::filter(labels, !(!!rlang::sym("child") %in% select_multiple_child_not_in_design))

  proportions <- purrr::map2(select_multiple_child_in_design, labels[["name"]], \(x,y) {

    # Get proportion per choice
    proportion <- svy_mean(design, var = !!rlang::sym(x), group = group, group_key_sep = group_key_sep, stat_name = "prop", vartype = vartype, na_rm = na_rm, level = level)

    # Remove the overall name and replace by child choices only
    proportion <- impactR.utils::deselect(proportion, "var")
    dplyr::mutate(proportion, "var_value" = {{ y }}, .before = dplyr::all_of("stat"))

  })

  # Bind the list
  proportions <- dplyr::bind_rows(proportions)

  # Statistic type is proportion
  proportions[["stat_type"]] <- "proportion"

  # Type of analysis is select_multiple
  proportions[["analysis"]] <- "select_multiple"

  proportions <- dplyr::mutate(proportions, "var" := var_name, .before = dplyr::all_of("var_value"))

  if (label_survey) {

    label <- impactR.kobo::get_survey_labels(survey, !!rlang::sym(var_name), output_df = FALSE)
    proportions <- dplyr::mutate(proportions, "var_label" = ifelse(length(label) == 0, NA_character_, label))

  }

  if (label_choices) {

    labels <- dplyr::rename(labels, "var_value_label" = !!rlang::sym("label"))
    labels <- dplyr::mutate(labels, "name" = as.character(!!rlang::sym("name")))
    labels <- impactR.utils::deselect(labels, "child", "col")

    proportions <- dplyr::mutate(proportions, "var_value" = as.character(!!rlang::sym("var_value")))
    proportions <- dplyr::left_join(proportions, labels, by = c("var_value" = "name"))

  }

  return(proportions)

}

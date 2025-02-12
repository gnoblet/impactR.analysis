#' Get the proportion for a select_multiple
#'
#' @param design A srvyr::design object.
#' @param vars A qupted variable to calculate proportion from.
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
#' @section Not removing missing values:
#'
#' The rationale when not removing missing values is the following:
#'
#' * Missing values for each dummy 1/0 column corresponding to response option are recoded to 0
#' * Then, the mean of 0s and 1s is computed, thus the % for all response options
#'
#' This allows to calculate the % for each choice over the whole dataset.
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A character vector of select_one questions.
#'
#' @export
kobo_select_multiple <- function(design, vars, survey, choices = NULL, choices_sep = "/", group = NULL, group_key_sep = " -/- ", label_survey = TRUE, label_choices = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95){

  #------ Check

  # Check if vars are in design
  checkmate::assertSubset(vars, colnames(design))

  #Checj if survey contains the necessary columns
  checkmate::assertSubset(c("type", "name"), colnames(survey))

  # Check if choices contains the necessary columns
  if (!is.null(choices)) checkmate::assertSubset(c("label", "name"), colnames(choices))

  # Check if vars are indeed select multiples (restrictive for a reason)
  select_multiples <- impactR.kobo::get_survey_select_multiple(survey)

  if (any(!(vars %in% select_multiples))) {

    vars_not_select_multiple <- vars[!(vars %in% select_multiples)]

    rlang::abort(
      c("Variable is not a select_multiple in survey.",
        "i" = glue::glue("You may check that column ", glue::glue_collapse(vars_not_select_multiple, sep = ", ", last = " and "), " is/are a `select_multiple` in 'survey'. Maybe verify that the survey sheet is the right and most updated version.")
      ))

  }

  # select_multiples that are a grouping column
  # not needed for a select multiple, right, has it should be for child columns
  # Should we check for child columns? / most likely but also unlikely to be a use and break, ok for now
  # if (var %in% group) rlang::abort("Grouping columns in `group` should be different than `var`.")

  #------ Body

  # Should an option to calculate the select multiple over the whole dataset, i.e., replacing NA by 0 be done?
  # Yes


  make_kobo_select_multiple <- function(design, var, survey, choices, choices_sep, group, group_key_sep, label_survey, label_choices, na_rm, vartype, level){

    # Get child columns
    select_multiple_child <- impactR.kobo::get_survey_choices(survey, choices, !!rlang::sym(var), sep = choices_sep)

    # Keep only childs that are in the datasets (on purpose)
    select_multiple_child_in_design <- impactR.utils::subvec_in(select_multiple_child, colnames(design))

    # STOP if there is not child column found in the dataset
    if (length(select_multiple_child_in_design) == 0) rlang::abort(
      c("No child column.",
        "*" = glue::glue("There is no child column found for column '{var}' in `design`. The 1/0 child columns are needed for calculation."),
        "i" = glue::glue("You may check that column '{var}' is a `select_multiple` question. You may also check that the provided separator for choices is the right one (arg 'choices_sep' default to '/'). Finally, you can verify that the survey sheet is the right and most updated version.")
      ))

    # if SOME child columns are in survey but not in the dataset, warn that they were discarded.
    select_multiple_child_not_in_design <- impactR.utils::subvec_not_in(select_multiple_child, select_multiple_child_in_design)
    if (length(select_multiple_child_not_in_design) > 0) rlang::warn(glue::glue(paste0("The following child columns of '{var}' does not exist in `design`, there are removed of the calculation: ", paste(
      select_multiple_child_not_in_design,
      collapse = ", "))))

    # Get labels for later and then unite the name and column to grab the child columns and filter out the non-existent ones
    labels <- impactR.kobo::get_survey_choices(survey, choices, !!rlang::sym(var), sep = choices_sep, label = TRUE)
    labels <- tidyr::unite(labels, "child", "col", "name", sep = choices_sep, remove = FALSE)
    labels <- dplyr::filter(labels, !(!!rlang::sym("child") %in% select_multiple_child_not_in_design))

    # If na_rm, recoded missing values to zero
    if (!na_rm) design <- srvyr::mutate(
      design, srvyr::across(
        dplyr::all_of(select_multiple_child_in_design),
        \(x) tidyr::replace_na(x, 0)
      )
    )

    # Calculate proportions as means of 0/1 dummy columns
    proportions <- purrr::map2(select_multiple_child_in_design, labels[["name"]], \(x,y) {

      # Get proportion per choice
      proportion <- svy_mean(design, vars = x, group = group, group_key_sep = group_key_sep, vartype = vartype, na_rm = na_rm, level = level)

      # Remove the overall name and replace by child choices only
      proportion <- impactR.utils::deselect(proportion, "var")
      dplyr::mutate(proportion, "var_value" = y, .before = dplyr::all_of("stat"))

    })

    # Bind the list
    proportions <- dplyr::bind_rows(proportions)

    # Statistic type is proportion
    # proportions[["stat_type"]] <- "proportion"
    # To be seen with RD and update the analysis key accordingly?

    # Type of analysis is select_multiple
    proportions[["analysis"]] <- "select_multiple"

    proportions <- dplyr::mutate(proportions, "var" := var, .before = dplyr::all_of("var_value"))

    if (label_survey) {

      label <- impactR.kobo::get_survey_labels(survey, !!rlang::sym(var), output_df = FALSE)
      proportions <- dplyr::mutate(proportions, "var_label" := ifelse(length(label) == 0, NA_character_, label))

    }

    if (label_choices) {

      labels <- dplyr::rename(labels, "var_value_label" := !!rlang::sym("label"))
      labels <- dplyr::mutate(labels, "name" := as.character(!!rlang::sym("name")))
      labels <- impactR.utils::deselect(labels, "child", "col")

      proportions <- dplyr::mutate(proportions, "var_value" := as.character(!!rlang::sym("var_value")))
      proportions <- dplyr::left_join(proportions, labels, by = c("var_value" = "name"))

    }

    return(proportions)

  }

  analysis <- purrr::map(
    vars,
    \(x) {

      an <- make_kobo_select_multiple(design = design, var = x, survey = survey, choices = choices, choices_sep = choices_sep, group = group, group_key_sep = group_key_sep, label_survey = label_survey, label_choices = label_choices, na_rm = na_rm, vartype = vartype, level = level)

      an <- dplyr::mutate(an, "var_value" := as.character(!!rlang::sym("var_value")))
    },
    .progress = TRUE
  )

  analysis <- purrr::list_rbind(analysis)

  return(analysis)

}

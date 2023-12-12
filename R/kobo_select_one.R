#' Get the proportion for a select_one
#'
#' @param design A srvyr::design object.
#' @param vars Variables to calculate proportion from.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param choices The choices sheet from Kobo. If not NULL, the function tries to retrieve labels.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve var's label from the survey sheet? Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @inheritParams svy_proportion
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A character vector of select_one questions.
#'
#' @export
kobo_select_one <- function(design, vars, survey, choices = NULL, group = NULL, group_key_sep = " ~/~ ", label_survey = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95){


  #------ Gather arguments

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  #------ Checks

  # Check if vars are in design
  if_not_in_stop(design, vars, "design")


  # Check survey and choices columns
  if_not_in_stop(survey, c("type", "name"), "survey")
  if (!is.null(choices)) if_not_in_stop(choices, c("label", "name"), "choices")

  # Checks already in svy_proportion()
  # - col and group existence, group and col identical
  # - level below 0.9 warning

  # TO DO:
  # - Add checks on survey if need, see with impactR.kobo
  # Should it check if the variable is a select one? i would say no, intended use, just labels will be empty
  # Now it is, see select_multiple
  select_ones <- impactR.kobo::get_survey_select_one(survey)

  # select_multiples that exists in design
  if (any(!(vars %in% select_ones))) {

    vars_not_select_one <- vars[!(vars %in% select_ones)]

    rlang::abort(
    c("Variable is not a select_one in survey.",
      "i" = glue::glue("You may check that column ", glue::glue_collapse(vars_not_select_one, sep = ", ", last = " and "), " is/are a `select_one` in 'survey'. Maybe verify that the survey sheet is the right and most updated version.")
    ))

  }

  # Calculate proportion
  proportion <- svy_proportion(design, vars, group =  group, vartype = vartype, level = level, na_rm = na_rm)

  # Type of analysis is select_multiple
  proportion[["analysis"]] <- "select_one"

  if (label_survey) {

    label <- impactR.kobo::get_survey_labels(survey, !!!vars, output_df = TRUE)
    label <- dplyr::rename(label, "var_label" = !!rlang::sym("label"))
    proportion <- dplyr::left_join(proportion, label, by = c("var" = "name"))
    # proportion <- dplyr::mutate(proportion, "var_label" = ifelse(length(label) == 0, NA_character_, label))

  }

  if (!is.null(choices)) {

    labels <- purrr::map(vars, \(x) {

      lab <- impactR.kobo::get_survey_choices(survey, choices, !!x, label = TRUE)
      lab <- dplyr::mutate(
        lab,
        "label" := as.character(!!rlang::sym("label")),
        "name" := as.character(!!rlang::sym("name")))

    })

    labels <- purrr::list_rbind(labels)
    labels <- dplyr::rename(labels, "var_value_label" := !!rlang::sym("label"))

    proportion <- dplyr::left_join(proportion, labels, by = c("var" = "col", "var_value" = "name"))


  }

  return(proportion)

}

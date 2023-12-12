#' Get the ratio for numeric variables.
#'
#' @param design A srvyr::design object.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve var's label from the survey sheet? Default to TRUE.
#'
#' @details `survey` should have a split type column with types of variables such as "select_one", "select_multiple", etc.
#'
#' @inheritParams svy_ratio
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A character vector of select_one questions.
#'
#' @export
kobo_ratio <- function(design, nums, denoms, ratio_key_sep = " ~/~ ", survey, group = NULL, group_key_sep = " ~/~ ", label_survey = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95){


  #------ Gather arguments

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  #------ Checks

  # Check if vars are in design
  if_not_in_stop(design, nums, "design")
  if_not_in_stop(design, denoms, "design")
  vars <- c(nums, denoms)


  # Check survey and choices columns
  if_not_in_stop(survey, c("type", "name"), "survey")

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
  ratio <- svy_ratio(design, nums, denoms, group =  group, vartype = vartype, level = level, na_rm = na_rm)

  # Type of analysis is select_multiple
  ratio[["analysis"]] <- "ratio"

  if (label_survey) {

    label_nums <- impactR.kobo::get_survey_labels(survey, !!!nums, output_df = TRUE)
    label_denoms <- impactR.kobo::get_survey_labels(survey, !!!denoms, output_df = TRUE)

    ratio <- tidyr::separate_wider_delim(ratio, !!rlang::sym("var"), delim = ratio_key_sep, names = c("var_num", "var_denom"), cols_remove = FALSE)

    label_nums <- dplyr::rename(label_nums, "var_label_num" := !!rlang::sym("label"))
    label_denoms <- dplyr::rename(label_denoms, "var_label_denom" := !!rlang::sym("label"))

    ratio <- dplyr::left_join(ratio, label_nums, by = c("var_num" = "name"))
    ratio <- dplyr::left_join(ratio, label_denoms, by = c("var_denom" = "name"))

    ratio <- tidyr::unite(ratio, "var_label", !!rlang::sym("var_label_num"), !!rlang::sym("var_label_denom"), sep = ratio_key_sep)

    ratio <- dplyr::select(ratio, !dplyr::all_of(c("var_num", "var_denom")))

  }


  return(ratio)

}

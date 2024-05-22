#' @title Kobo survey analysis
#'
#' @param design A srvyr::design object.
#' @param analysis Analysis type. See details for more, well, details.
#' @param vars A variable to calculate from.
#' @param choices The choices sheet from Kobo.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `var` be removed? Default to TRUE.
#'
#' @section Specificity per type of analysis:
#'
#' The possible analysis type are: mean for [kobo_mean()], median for [kobo_median()], select_one for [kobo_select_one()], select_multiple for [kobo_select_multiple()], or ratio for [kobo_ratio()].
#'
#' * If `analysis` is "ratio": `vars` takes a named vector. Names will be passed to `nums` in [svy_ratio()] and values to `denoms`.
#' * If choices not NULL, the function tries to retrieve labels for select_one, while it is mandatory for select_multiple, and not used for mean, median, and ratio.
#'
#' @inheritParams kobo_select_multiple
#' @inheritParams kobo_ratio
#'
#' @importFrom rlang `:=`
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A survey-summarized-median data frame
#'
#' @export
#'
kobo_analysis <- function(design, analysis, vars, survey, choices = NULL, group = NULL, group_key_sep = " -/- ", label_survey = TRUE, label_choices = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95, ratio_key_sep = " -/- ",  choices_sep = "/"){


  analysis_type <- c("mean", "median", "select_multiple", "select_one", "ratio")

  if (!(analysis %in% analysis_type)) rlang::abort(paste0("Please provide an analysis from the following list: ", paste(analysis_type, collapse = ", "), "."))

  if (analysis == "mean") {

    an <- kobo_mean(design, vars = vars, survey = survey, group = group, group_key_sep = group_key_sep, label_survey = label_survey, na_rm = na_rm, vartype = vartype, level = level)

  } else if (analysis == "median") {

    an <- kobo_median(design, vars = vars, survey = survey, group = group, group_key_sep = group_key_sep, label_survey = label_survey, na_rm = na_rm, vartype = vartype, level = level)

  } else if (analysis == "select_one") {

    an <- kobo_select_one(design, vars = vars, survey = survey, choices = choices, group = group, group_key_sep = group_key_sep, label_survey = label_survey, na_rm = na_rm, vartype = vartype, level = level)

  } else if (analysis == "ratio") {

    nums <- names(vars)
    denoms <- unname(vars)
    an <- kobo_ratio(design, nums, denoms, ratio_key_sep = ratio_key_sep, survey = survey, group = group, group_key_sep = group_key_sep, label_survey = label_survey, na_rm = na_rm, vartype = vartype, level = level)

  } else if (analysis == "select_multiple") {

    if (is.null(choices)) rlang::abort("For the 'select multiple' analysis type, please provide the choices sheet.")

    an <- kobo_select_multiple(design, vars = vars, survey = survey, choices = choices, choices_sep = choices_sep, group = group, group_key_sep = group_key_sep, label_survey = label_survey, na_rm = na_rm, vartype = vartype, level = level)

  }  else {

    rlang::abort(paste0("Analysis ", analysis, "is not implemented yet... or will not. Feel free to reach out!"))
  }

  return(an)

}

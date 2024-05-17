#' @title Survey analysis
#'
#' @param design A srvyr::design object.
#' @param analysis Analysis type. See details for more, well, details.
#' @param vars A variable to calculate from.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `var` be removed? Default to TRUE.
#' @param ... Other parameters to pass to the used `srvyr` method.
#'
#' @section Specificity per type of analysis:
#'
#' The possible analysis type are: mean for [svy_mean()], median for [svy_median()], interact for [svy_interact()], proportion for [svy_proportion()], ratio for [svy_ratio()], or quantile [svy_quantile()].
#'
#' * If `analysis` is "ratio": `vars` takes a named vector. Names will be passed to `nums` in [svy_ratio()] and values to `denoms`.
#' * If `analysis` is "interact": `vars` is the equivalent of `interact`.
#'
#' @inheritParams svy_ratio
#' @inheritParams svy_interact
#' @inheritParams svy_quantile
#'
#' @importFrom rlang `:=`
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-median data frame
#'
#' @export
#'
svy_analysis <- function(design, analysis, vars, group = NULL, group_key_sep = " -/- ", na_rm = TRUE, vartype = "ci", level = 0.95, ratio_key_sep = " ~/~ ",  interact_key_sep = " -/- ", quantiles = c(0.25, 0.5, 0.75), ak = TRUE, ak_overall_sep = " @/@ ", ak_main_sep = " -/-", ak_var_to_value_sep = " %/% ", ...){


  analysis_type <- c("mean", "median", "proportion", "quantile", "ratio", "interact")

  if (!(analysis %in% analysis_type)) rlang::abort(paste0("Please provide an analysis from the following list: ", paste(analysis_type, collapse = ", "), "."))

  if (analysis == "mean") {

    an <- svy_mean(design, vars, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ak = ak, ak_overall_sep = ak_overall_sep, ak_main_sep = ak_main_sep, ak_var_to_value_sep = ak_var_to_value_sep, ...)

  } else if (analysis == "median") {

    an <- svy_mean(design, vars, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ak = ak, ak_overall_sep = ak_overall_sep, ak_main_sep = ak_main_sep, ak_var_to_value_sep = ak_var_to_value_sep,...)

  } else if (analysis == "proportion") {

    an <- svy_proportion(design, vars, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ak = ak, ak_overall_sep = ak_overall_sep, ak_main_sep = ak_main_sep, ak_var_to_value_sep = ak_var_to_value_sep,...)

  } else if (analysis == "ratio") {

    nums <- names(vars)
    denoms <- unname(vars)
    an <- svy_ratio(design, nums = nums, denoms = denoms, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ak = ak, ak_overall_sep = ak_overall_sep, ak_main_sep = ak_main_sep, ak_var_to_value_sep = ak_var_to_value_sep,...)

  } else if (analysis == "interact") {

    an <- svy_interact(design, interact = vars, interact_key_sep = interact_key_sep, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ak = ak, ak_overall_sep = ak_overall_sep, ak_main_sep = ak_main_sep, ak_var_to_value_sep = ak_var_to_value_sep,...)

  } else if (analysis == "quantile") {

    an <- svy_quantile(design, vars = vars, quantiles = quantiles, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level,...)

    if (ak) rlang::warn("The analysis key has not been implemented for 'quantile'.")

  }  else {
    rlang::abort(paste0("Analysis ", analysis, "is not implemented yet... or will not. Feel free to reach out!"))
  }

  return(an)

}

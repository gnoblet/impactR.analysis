#' @title Auto survey from a Kobo tool analysis
#'
#' @param design A srvyr::design object.
#' @param survey A survey sheet.
#' @param choices A choices sheet.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `var` be removed? Default to TRUE.
#' @param bind Boolean. Output a dataframe or a list?
#'
#' @section Automation:
#'
#' [auto_kobo_analysis] performs a very basic automated analysis using the provided Kobo tool.
#'
#' @inheritParams kobo_select_multiple_all
#'
#' @importFrom rlang `:=`
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A survey-summarized-median data frame
#'
#' @export
#'
auto_kobo_analysis <- function(design, survey, choices, group = NULL, group_key_sep = " ~/~ ", na_rm = TRUE, vartype = "ci", level = 0.95, choices_sep = "_", label_survey = TRUE, bind = TRUE){

  num_median_an <- kobo_median_all(design, survey, group, group_key_sep, label_survey, na_rm, vartype, level)
  num_mean_an <- kobo_mean_all(design, survey, group, group_key_sep, label_survey, na_rm, vartype, level)

  select_ones_an <- kobo_select_one_all(design, survey, choices, group, group_key_sep, label_survey, na_rm, vartype, level)

  select_multiples_an <- kobo_select_multiple_all(design, survey, choices, group, group_key_sep, choices_sep, label_survey, label_choices = TRUE, na_rm, vartype, level)

  an <- list(num_median_an, num_mean_an, select_ones_an, select_multiples_an)


  if (!bind) {

    return(an)

  } else {

    an <- purrr::list_rbind(an)

    return(an)
  }

}

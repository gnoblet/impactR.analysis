#' @title Auto survey analysis
#'
#' @param design A srvyr::design object.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `var` be removed? Default to TRUE.
#' @param bind Boolean. Output a dataframe or a list?
#'
#' @section Automation:
#'
#' [auto_svy_analysis] performs a very basic automated analysis. All numeric columns get means and medians. All character columns get a proportion.
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
auto_svy_analysis <- function(design, group = NULL, group_key_sep = " -/- ", na_rm = TRUE, vartype = "ci", level = 0.95, bind = TRUE){

  numeric_cols <- colnames(design$variables)[purrr::map_lgl(design$variables, \(x) is.numeric(x))]

  numeric_analysis_type <- c("mean", "median")
  numeric_an <- purrr::map(numeric_analysis_type, \(x) svy_analysis(design, x, vars = numeric_cols, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level), .progress = "numeric")

  an <- purrr::set_names(numeric_an, numeric_analysis_type)

  character_cols <- colnames(design$variables)[purrr::map_lgl(design$variables, \(x) is.character(x))]

  an["prop"] <- purrr::map("proportion", \(x) svy_analysis(design, x, vars = character_cols, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level), .progress = "character")


  if (!bind) {

    return(an)

  } else {
    an <- purrr::list_rbind(an)

    return(an)
  }

}

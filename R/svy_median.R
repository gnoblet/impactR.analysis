#' @title Survey median
#'
#' @param design A srvyr::design object.
#' @param col A column to calculate median from.
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE.
#' @param stat_name What should the statistic's column be named? Default to "median".
#' @param ... Other parameters to pass to `srvyr::survey_median()`.
#'
#' @inheritParams srvyr::survey_median
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-median data frame
#'
#' @export
svy_median <- function(design, col, group = NULL, na_rm = TRUE, stat_name = "median", vartype = "ci", level = 0.95, deff = FALSE, ...){

  # Get col name
  col_name <- rlang::as_name(rlang::enquo(col))

  # Get number of NAs
  na_count_tot <- sum(is.na(srvyr::pull(design, {{ col }})))
  n_tot <- nrow(design)

  # Remove NAs
  if (na_rm) design <- srvyr::drop_na(design, {{ col }})

  # Group design for calculation
  to_return <- srvyr::group_by(design, srvyr::across({{ group }}))

  # Summarize design
  # - stat_name: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "{stat_name}" := srvyr::survey_median({{ col }}, vartype = vartype, level = level, deff = deff, ...),
    "{stat_name}_unw" := srvyr::unweighted(stats::median({{ col }})),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Regroup by group to calculate unweighted total by groups
  to_return <- dplyr::group_by(to_return, dplyr::across({{ group }}))

  # Get unweighted total
  to_return <- dplyr::mutate(to_return, "n_tot_unw" := !!rlang::sym("n_unw"))

  # Ungroup
  to_return <- dplyr::ungroup(to_return)

  # Add total number of obs and total number od NAs
  to_return <- dplyr::mutate(
    to_return,
    "n_tot" = n_tot,
    "na_count_tot" = na_count_tot)

  return(to_return)
}

#' @title Survey quantile
#'
#' @param design A srvyr::design object.
#' @param col A column to calculate quantile from.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE.
#' @param ... Other parameters to pass to `srvyr::survey_mean()`.
#'
#' @inheritParams srvyr::survey_mean
#'
#' @importFrom rlang `:=`
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-mean data frame
#'
#' @export
svy_quantile <- function(design, col, quantiles = c(0.25, 0.5, 0.75), group = NULL, group_key_sep = "*", na_rm = TRUE, stat = "quantile", vartype = "ci", level = 0.95, ...){

  # Get col name
  col_name <- rlang::as_name(rlang::enquo(col))

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  # Check if col is not a grouping column
  if (col_name %in% group) rlang::abort("Grouping columns in `group` should be different than `col`.")

  # Get number of NAs
  na_count_tot <- sum(is.na(srvyr::pull(design, {{ col }})))
  n_tot <- nrow(design)

  # Remove NAs
  if (rlang::is_true(na_rm)) design <- srvyr::drop_na(design, {{ col }})

  # Group design for calculation
  to_return <- srvyr::group_by(design, srvyr::across({{ group }}))

  # Summarize design
  # - stat: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "stat" := srvyr::survey_quantile(!!rlang::sym(col_name), quantiles = quantiles, vartype = vartype, level = level, ...),
    "stat_unw" := srvyr::unweighted(mean(!!rlang::sym(col_name))),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Add stat type
  to_return[["stat_type"]] <- "quantile"

  return(to_return)

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

  # Return column name
  to_return <- dplyr::mutate(
    to_return,
    var = col_name,
    .before = dplyr::all_of("stat"))

  if (group_key != "") {to_return <- add_group_key(to_return, group, group_key, group_key_sep, before = "var")}

  return(to_return)
}

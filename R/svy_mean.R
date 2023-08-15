#' @title Survey mean
#'
#' @param design A srvyr::design object.
#' @param var A variable to calculate the mean from.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `var` be removed? Default to TRUE.
#' @param stat_name What should the statistic's column be named? Default to "mean".
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
svy_mean <- function(design, var, group = NULL, group_key_sep = "*", na_rm = TRUE, stat_name = "mean", vartype = "ci", level = 0.95, ...){

  #------ Gather arguments

  # Get col name
  var_name <- rlang::as_name(rlang::enquo(var))

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  #------ Checks

  # Check if design is a design
  if (!("tbl_svy") %in% class(design)) rlang::abort("'design' is not a `tbl_svy` object.")

  # Check if var is in design
  if_not_in_stop(design, var_name, df_name = "design", arg = "var")

  # Check if group cols are in design
  if_not_in_stop(design, group, df_name = "design", arg = "group")

  # Check if var is not a grouping column
  if (var_name %in% group) rlang::abort("Grouping columns in `group` should be different than `var`.")

  # Warn on the CI level:
  if (level < 0.9 & vartype == "ci"){rlang::warn("The confidence level used  is below 90%.")}

  #------ Body

  # Get number of NAs
  na_count_tot <- sum(is.na(srvyr::pull(design, {{ var }})))
  n_tot <- nrow(design)

  # Remove NAs
  if (rlang::is_true(na_rm)) design <- srvyr::drop_na(design, {{ var }})

  # Group design for calculation
  to_return <- srvyr::group_by(design, srvyr::across({{ group }}))

  # Summarize design
  # - stat: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "stat" := srvyr::survey_mean(!!rlang::sym(var_name), vartype = vartype, level = level, ...),
    "stat_unw" := srvyr::unweighted(mean(!!rlang::sym(var_name))),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Add stat type
  to_return[["stat_type"]] <- "mean"

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
    var = var_name,
    .before = dplyr::all_of("stat"))

  # Get the group keys and values
  if (group_key != "") {to_return <- add_group_key(to_return, group, group_key, group_key_sep, before = "var")}

  return(to_return)
}

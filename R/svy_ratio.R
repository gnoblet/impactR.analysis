#' @title Survey ratio
#'
#' @param design A srvyr::design object.
#' @param num The numerator variable.
#' @param denom The denominator variable.
#' @param ratio_key_sep A separator for the ratio key.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Boolean. Remove any line that as an NA in `num` or `denom` Default to TRUE.
#' @param ... Parameters to pass to `srvyr::survey_ratio()`.
#'
#' @importFrom rlang `:=`
#'
#' @inheritParams srvyr::survey_ratio
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-ratio data frame
#'
#' @export
svy_ratio <- function(design, num, denom, ratio_key_sep = " / ", group = NULL, group_key_sep = "*", na_rm = TRUE, vartype = "ci", level = 0.95, ...){

  #------ Gather arguments

  # Get var names
  num_name <- rlang::as_name(rlang::enquo(num))
  denom_name <- rlang::as_name(rlang::enquo(denom))

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  #------ Checks

  # Check if design is a design
  if (!("tbl_svy") %in% class(design)) rlang::abort("'design' is not a `tbl_svy` object.")

  # Check if num are in design
  if_not_in_stop(design, num_name, df_name = "design", arg = "num")

  # Check if denom are in design
  if_not_in_stop(design, denom_name, df_name = "design", arg = "denom")

  # Check if group cols are in design
  if_not_in_stop(design, group, df_name = "design", arg = "group")

  # Check if col is not a grouping column
  if (any(c(num_name, denom_name) %in% group)) rlang::abort("Grouping columns in `group` should be different than `num` or `denom`.")

  # Warn on the CI level:
  if (level < 0.9 & vartype == "ci"){rlang::warn("The confidence level used  is below 90%.")}

  #------ Body

  # Get number of NAs -- for ratio it either for num or denom
  na_count_tot <- sum(is.na(srvyr::pull(design, {{ denom }})) | is.na(srvyr::pull(design, {{ num }})))
  n_tot <- nrow(design)

  if (na_rm) {
    design <- srvyr::drop_na(design, {{ num }})
    design <- srvyr::drop_na(design, {{ denom }})
  }

  # Group design for calculation
  to_return <- srvyr::group_by(design, srvyr::across({{ group }}))

  # Summarize design
  # - stat: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "stat" := srvyr::survey_ratio(!!rlang::sym(num_name), !!rlang::sym(denom_name), ...),
    "stat_unw" := srvyr::unweighted(sum(!!rlang::sym(num_name)) / sum(!!rlang::sym(denom_name))),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Add stat type
  to_return[["stat_type"]] <- "ratio"

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
    var = paste(num_name, denom_name, sep = ratio_key_sep),
    .before = dplyr::all_of("stat"))

  # Get the group keys and values
  if (group_key != "") {to_return <- add_group_key(to_return, group, group_key, group_key_sep, before = "var")}

  return(to_return)
}

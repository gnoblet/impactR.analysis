#' @title Survey ratio
#'
#' @param design A srvyr::design object.
#' @param nums The numerator variables.
#' @param denoms The denominator variables.
#' @param ratio_key_sep A separator for the ratio key.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Boolean. Remove any line that as an NA in `num` or `denom` Default to TRUE.
#' @param ak Boolean. Add the analysis key?
#' @param ak_overall_sep The overall separator between items, e.g. between the type of analysis and the variables information.
#' @param ak_main_sep The main separator between variables, e.g. between the two grouping columns.
#' @param ak_var_to_value_sep The separator between the variable and its value.
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
svy_ratio <- function(design, nums, denoms, ratio_key_sep = " -/- ", group = NULL, group_key_sep = " -/- ", na_rm = TRUE, vartype = "ci", level = 0.95, ak = TRUE, ak_overall_sep = " @/@ ", ak_main_sep = " -/- ", ak_var_to_value_sep = " %/% ", ...){

  #------ Gather arguments

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)


  #------ Checks

  # Check if design is a design
  if (!("tbl_svy") %in% class(design)) rlang::abort("'design' is not a `tbl_svy` object.")

  # Check if both length are the same
  if (length(nums) != length(denoms)) rlang::abort("Lengths of `nums` and `denoms` are different. Please provide the same number of numerators and denominators.")

  # Check if num are in design
  checkmate::assertSubset(nums, colnames(design))

  # Check if denom are in design
  checkmate::assertSubset(denoms, colnames(design))

  # Check if group cols are in design
  if (!is.null(group)) checkmate::assertSubset(group, colnames(design))

  # Check if col is not a grouping column
  if (any(c(nums, denoms) %in% group)) rlang::abort("Grouping columns in `group` should be different than `nums` or `denoms`.")

  # Warn on the CI level:
  if (level < 0.9 & vartype == "ci"){rlang::warn("The confidence level used  is below 90%.")}


  #------ Body

  make_ratio <- function(design, num, denom, ratio_key_sep, group, group_key, group_key_sep, na_rm, vartype, level, ...){

    # if all NA, return empty tibble
    if (all(is.na(c(srvyr::pull(design, !!rlang::sym(num)), srvyr::pull(design, !!rlang::sym(denom)))))) {
      rlang::warn(paste0("Variable '", num, "' and variable '", denom, "' only contains missing values. Returning an empty data frame."))
      return(dplyr::tibble())  # Return an empty data.frame
    }

    # Get number of NAs -- for ratio it either for num or denom
    na_count_tot <- sum(is.na(srvyr::pull(design, !!rlang::sym(denom))) | is.na(srvyr::pull(design, !!rlang::sym(num))))
    n_tot <- nrow(design)

    if (na_rm) {
      design <- srvyr::drop_na(design,!!rlang::sym(num))
      design <- srvyr::drop_na(design, !!rlang::sym(denom))
    }

    # Group design for calculation
    to_return <- srvyr::group_by(design, srvyr::across(dplyr::all_of(group)))

    # Summarize design
    # - stat: the weighted proportion of obs
    # - n_unw: the unweighted count of obs
    to_return <- srvyr::summarize(
      to_return,
      "stat" := srvyr::survey_ratio(!!rlang::sym(num), !!rlang::sym(denom), vartype = vartype, ...),
      "stat_unw" := srvyr::unweighted(sum(!!rlang::sym(num)) / sum(!!rlang::sym(denom))),
      "n_unw" := srvyr::unweighted(srvyr::n()))

    # Add stat type
    to_return[["stat_type"]] <- "ratio"

    # Regroup by group to calculate unweighted total by groups
    to_return <- dplyr::group_by(to_return, dplyr::across(dplyr::all_of(group)))

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
      var = paste(num, denom, sep = ratio_key_sep),
      .before = dplyr::all_of("stat"))

    # Get the group keys and values
    if (group_key != "") {to_return <- add_group_key(to_return, group, group_key, group_key_sep, before = "var")}
    if (group_key == "") {to_return <- dplyr::mutate(to_return, group_key = NA_character_, group_key_value = NA_character_, .before = "var")}

    return(to_return)
  }

  analysis <- purrr::map2(
    nums,
    denoms,
    \(x,y) make_ratio(design, x, y, ratio_key_sep = ratio_key_sep, group = group, group_key = group_key, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ...)
  )

  analysis <- purrr::list_rbind(analysis)

  # if analysis is empty df, return
  if (nrow(analysis) == 0) return(analysis)


  analysis <- dplyr::mutate(analysis, var_value = NA_character_, .after = "var")

  # Add the analysis key
  if(ak) analysis <- add_analysis_key(analysis, group_key_sep = group_key_sep, var_key_sep = ratio_key_sep, overall_sep = ak_overall_sep, main_sep =  ak_main_sep, var_to_value_sep = ak_var_to_value_sep)


  return(analysis)
}

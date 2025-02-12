#' @title Survey quantile
#'
#' @param design A srvyr::design object.
#' @param vars A quoted vector of variables to calculate quantile from.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `var` be removed? Default to TRUE.
#' @param ... Other parameters to pass to `srvyr::survey_quantile()`.
#'
#' @inheritParams srvyr::survey_quantile
#'
#' @importFrom rlang `:=`
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-mean data frame
#'
#' @export
svy_quantile <- function(design, vars, quantiles = c(0.25, 0.5, 0.75), group = NULL, group_key_sep = " -/- ", na_rm = TRUE, vartype = "ci", level = 0.95, ...){


  #------ Gather arguments

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)


  #------ Checks

  # Check if design is a design
  if (!("tbl_svy") %in% class(design)) rlang::abort("'design' is not a `tbl_svy` object.")

  # Check if var are in design
  checkmate::assertSubset(vars, colnames(design))

  # Check if group cols are in design
  if (!is.null(group)) checkmate::assertSubset(group, colnames(design))
  
  # Check if var is not a grouping column
  #--- simple abortin check, that could become a warning
  if (any(vars %in% group)) rlang::abort("Grouping columns in `group` should be different than the ones in `vars`.")

  # Warn on the CI level:
  if (level < 0.9 & vartype == "ci"){rlang::warn("The confidence level used  is below 90%.")}


  #------ Body

  make_quantile <- function(design, var, quantiles, group, group_key, group_key_sep, na_rm, vartype, level, ...){

    # if all NA, return empty tibble
    if (all(is.na(srvyr::pull(design, !!rlang::sym(var))))) {
      rlang::warn(paste0("Variable '", var, "' only contains missing values. Returning an empty data frame."))
      return(dplyr::tibble())  # Return an empty data.frame
    }

    # Get number of NAs
    na_count_tot <- sum(is.na(srvyr::pull(design, !!rlang::sym(var))))
    n_tot <- nrow(design)

    # Remove NAs
    if (rlang::is_true(na_rm)) design <- srvyr::drop_na(design, !!rlang::sym(var))

    # Group design for calculation
    to_return <- srvyr::group_by(design, srvyr::across(dplyr::all_of(group)))

    # Summarize design
    # - stat: the weighted proportion of obs
    # - n_unw: the unweighted count of obs
    to_return <- srvyr::summarize(
      to_return,
      "stat" := srvyr::survey_quantile(!!rlang::sym(var), quantiles = quantiles, vartype = vartype, level = level, ...),
      "stat_unw" := srvyr::unweighted(mean(!!rlang::sym(var))),
      "n_unw" := srvyr::unweighted(srvyr::n()))

    # Add stat type
    to_return[["stat_type"]] <- "quantile"

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
      var = var,
      .before = dplyr::everything())

    if (group_key != "") {to_return <- add_group_key(to_return, group, group_key, group_key_sep, before = "var")}
    if (group_key == "") {to_return <- dplyr::mutate(to_return, group_key = NA_character_, group_key_value = NA_character_, .before = "var")}

    return(to_return)
  }

  analysis <- purrr::map(
    vars,
    \(x) make_quantile(design, x, quantiles = quantiles, group = group, group_key = group_key, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ...)
  )

  analysis <- purrr::list_rbind(analysis)

  return(analysis)

}

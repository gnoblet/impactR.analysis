#' @title Survey proportion
#'
#' @param design A srvyr::design object.
#' @param vars A quoted vector of variables to calculate proportion from.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `var` be removed? Default to TRUE.
#' @param ... Other parameters to pass to `srvyr::survey_prop()`.
#'
#' @inheritParams srvyr::survey_prop
#'
#' @importFrom rlang `:=`
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-proportion data frame
#'
#' @export
svy_prop <- function(design, vars, group = NULL, group_key_sep = "*", na_rm = TRUE, vartype = "ci", level = 0.95, ...){

  #------ Gather arguments

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)


  #------ Checks

  # Check if design is a design
  if (!("tbl_svy") %in% class(design)) rlang::abort("'design' is not a `tbl_svy` object.")

  # Check if var are in design
  if_not_in_stop(design, vars, df_name = "design", arg = "vars")

  # Check if group cols are in design
  if_not_in_stop(design, group, df_name = "design", arg = "group")

  # Check if var is not a grouping column
  #--- simple abortin check, that could become a warning
  if (any(vars %in% group)) rlang::abort("Grouping columns in `group` should be different than the ones in `vars`.")

  # Warn on the CI level:
  if (level < 0.9 & vartype == "ci"){rlang::warn("The confidence level used  is below 90%.")}


  #------ Body

  make_prop <- function(design, var, group, group_key, group_key_sep, na_rm, vartype, level, ...){

    # Get number of NAs
    na_count_tot <- sum(is.na(srvyr::pull(design, !!rlang::sym(var))))
    n_tot <- nrow(design)

    # Remove NAs
    if (na_rm) design <- srvyr::drop_na(design, !!rlang::sym(var))

    # Group design for calculation
    to_return <- srvyr::group_by(design, srvyr::across(dplyr::all_of({{ group }})), srvyr::across({{ var }}))

    # Summarize design
    # - stat: the weighted proportion of obs
    # - n_unw: the unweighted count of obs
    to_return <- srvyr::summarize(
      to_return,
      "stat" := srvyr::survey_prop(vartype = vartype, level = level, proportion = FALSE, ...),
      "n_unw" := srvyr::unweighted(srvyr::n()))

    # Get unweighted proportions
    to_return <- dplyr::mutate(to_return, "stat_unw" := prop.table(!!rlang::sym("n_unw")))

    # Add stat type
    to_return[["stat_type"]] <- "proportion"

    # Regroup by group to calculate unweighted total by groups
    to_return <- dplyr::group_by(to_return, dplyr::across({{ group }}))

    # Get unweighted total
    to_return <- dplyr::mutate(to_return, "n_tot_unw" := sum(!!rlang::sym("n_unw"), na.rm = FALSE))

    # Ungroup
    to_return <- dplyr::ungroup(to_return)

    # Add total number of obs and total number od NAs
    to_return <- dplyr::mutate(
      to_return,
      "n_tot" = n_tot,
      "na_count_tot" = na_count_tot)

    # Change values column name
    to_return <- dplyr::rename(to_return, "var_value" = !!rlang::sym(var))

    # Return column name
    to_return <- dplyr::mutate(
      to_return,
      var = var,
      .before = dplyr::all_of("var_value"))

    # Get the group keys and values
    if (group_key != "") {to_return <- add_group_key(to_return, group, group_key, group_key_sep, before = "var")}

    return(to_return)
  }

  analysis <- purrr::map(
    vars,
    \(x) make_prop(design, x, group = group, group_key = group_key, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level, ...)
  )

  analysis <- purrr::list_rbind(analysis)

  return(analysis)

}

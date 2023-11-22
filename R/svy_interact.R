#' @title Survey interactions
#'
#' @param design A srvyr::design object.
#' @param interact A quoted vector of variables to calculate interactions from (must be quoted).
#' @param interact_key_sep A character string to separate interactions columns in a fancy 'interact_key" column.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param unnest_interaction Should interaction be unnested? Default to TRUE.
#' @param na_rm Should NAs from `interact` be removed? Default to TRUE.
#' @param ak Boolean. Add the analysis key?
#' @param ak_overall_sep The overall separator between items, e.g. between the type of analysis and the variables information.
#' @param ak_main_sep The main separator between variables, e.g. between the two grouping columns.
#' @param ak_var_to_value_sep The separator between the variable and its value.
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
svy_interact <- function(design, interact, interact_key_sep = " ~/~ ", group = NULL, group_key_sep = " ~/~ ", unnest_interaction = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95, ak = TRUE, ak_overall_sep = " @/@ ", ak_main_sep = " ~/~ ", ak_var_to_value_sep = " %/% ", ...){

  #------ Gather arguments

  # Get interaction key
  interact_key <- paste(interact, collapse = interact_key_sep)

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

  #------ Checks

  # Check if design is a design
  if (!("tbl_svy") %in% class(design)) rlang::abort("'design' is not a `tbl_svy` object.")

  # Check if interact are in design
  if_not_in_stop(design, interact, df_name = "design", arg = "interact")

  # Check if group cols are in design
  if_not_in_stop(design, group, df_name = "design", arg = "group")

  # Check if col is not a grouping column
  if (any(interact %in% group)) rlang::abort("Grouping columns in `group` should be different than those in `interact`.")

  # Warn on the CI level:
  if (level < 0.9 & vartype == "ci"){rlang::warn("The confidence level used  is below 90%.")}

  #------ Body

  # Get number of rows
  n_tot <- nrow(design)

  # Remove NAs
  design_no_na <- srvyr::drop_na(design, {{ interact }})
  na_count_tot <- nrow(design) - nrow(design_no_na)
  if (na_rm) design <- design_no_na


  # Group design for calculation
  to_return <- srvyr::group_by(
    design,
    srvyr::across({{ group }}),
    srvyr::interact(interaction = srvyr::across({{ interact }})))

  # Summarize design
  # - stat: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "stat" := srvyr::survey_mean(vartype = vartype, level = level, ...),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Add stat type
  to_return[["stat_type"]] <- "interaction_proportion"

  # Get unweighted proportions
  to_return <- dplyr::mutate(to_return, "stat_unw" := prop.table(!!rlang::sym("n_unw")))

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

  if (unnest_interaction){

    to_return <- to_return |> tidyr::unnest("interaction")

    # Get the interact keys and values
    if (interact_key != "") {to_return <- add_interact_key(to_return, interact, interact_key, interact_key_sep, before = "stat")}
    if (interact_key == "") {to_return <- dplyr::mutate(to_return, interact_key = NA_character_, interact_key_value = NA_character_, .before = "stat")}


  }

  # Get the group keys and values
  if (group_key != "") {to_return <- add_group_key(to_return, group, group_key, group_key_sep, before = "stat")}
  if (group_key == "") {to_return <- dplyr::mutate(to_return, group_key = NA_character_, group_key_value = NA_character_, .before = "stat")}


  # Add the analysis key
  if(ak) to_return <- add_analysis_key(to_return, group_key_sep = group_key_sep, var_name = "interact_key", var_value_name = "interact_key_value", var_key_sep = interact_key_sep, overall_sep = ak_overall_sep, main_sep =  ak_main_sep, var_to_value_sep = ak_var_to_value_sep)


  return(to_return)
}

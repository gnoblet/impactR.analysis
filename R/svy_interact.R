#' @title Survey interactions
#'
#' @param design A srvyr::design object.
#' @param interact_cols A vector of columns to calculate interactions from (must be quoted).
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param key_sep A key separator for the output interaction column.
#' @param unnest_interaction Should interaction be unnested? Default to TRUE.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE.
#' @param stat_name What should the statistic's column be named? Default to "prop".
#' @param ... Other parameters to pass to `srvyr::survey_mean()`.
#'
#' @inheritParams srvyr::survey_mean
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-mean data frame
#'
#' @export
svy_interact <- function(design, interact_cols, group = NULL, key_sep = "*", unnest_interaction = TRUE, na_rm = TRUE, stat_name = "prop_interact", vartype = "ci", level = 0.95, deff = FALSE, ...){


  # Get column_names as a key
  key <- paste(interact_cols, collapse = key_sep)

  # Get number of rows
  n_tot <- nrow(design)

  # Remove NAs
  design_no_na <- srvyr::drop_na(design, {{ interact_cols }})
  na_count_tot <- nrow(design) - nrow(design_no_na)
  if (na_rm) design <- design_no_na


  # Group design for calculation
  to_return <- srvyr::group_by(
    design,
    srvyr::across({{ group }}),
    srvyr::interact(interaction = srvyr::across({{ interact_cols }})))

  # Summarize design
  # - stat_name: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "{stat_name}" := srvyr::survey_mean(vartype = vartype, level = level, deff = deff, ...),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Get unweighted proportions
  to_return <- dplyr::mutate(to_return, "{stat_name}_unw" := prop.table(.data$n_unw))

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

  # Add key
  to_return <- dplyr::mutate(to_return, "key" = key)

  if (unnest_interaction){
    to_return <- to_return |> tidyr::unnest("interaction")
  }

  return(to_return)
}

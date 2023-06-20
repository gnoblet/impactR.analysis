#' @title Survey proportion
#'
#' @param design A srvyr::design object.
#' @param col A column to calculate proportion from.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE.
#' @param stat_name What should the statistic's column be named? Default to "prop".
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
svy_prop <- function(design, col, group = NULL, group_key_sep = "*", na_rm = TRUE, stat_name = "prop", vartype = "ci", level = 0.95, ...){

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
  if (na_rm) design <- srvyr::drop_na(design, {{ col }})

  # Group design for calculation
  to_return <- srvyr::group_by(design, srvyr::across({{ group }}), srvyr::across({{ col }}))

  # Summarize design
  # - stat_name: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "{stat_name}" := srvyr::survey_prop(vartype = vartype, level = level, proportion = FALSE, ...),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Get unweighted proportions
  to_return <- dplyr::mutate(to_return, "{stat_name}_unw" := prop.table(rlang::.data$n_unw))

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
  to_return <- dplyr::rename(to_return, "value" = {{ col }})

  # Return column name
  to_return <- dplyr::mutate(
    to_return,
    name = col_name,
    .before = !!rlang::sym("value"))

  if (group_key != "") {
    # Add group key
    to_return[["group_key"]] <- group_key

    # Add group key values
    to_return[["group_key_value"]] <- do.call(paste, c(to_return[group], sep = group_key_sep))
    # to_return <- tidyr::unite(to_return, "group_key_value", tidyr::all_of(group), sep = group_key_sep, remove = FALSE)

    # Place group_key in front
    to_return <- dplyr::relocate(to_return, "group_key_value", .before = "name")
    to_return <- dplyr::relocate(to_return, "group_key", .before = "group_key_value")
  }

  return(to_return)
}

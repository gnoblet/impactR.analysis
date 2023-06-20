#' @title Survey interactions
#'
#' @param design A srvyr::design object.
#' @param interact A quoted vector of columns to calculate interactions from (must be quoted).
#' @param interact_key_sep A character string to separate interactions columns in a fancy 'interact_key" column.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param group_key_sep A character string to separate grouping column names in a fancy 'group_key' column.
#' @param unnest_interaction Should interaction be unnested? Default to TRUE.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE.
#' @param stat_name What should the statistic's column be named? Default to "prop".
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
svy_interact <- function(design, interact, interact_key_sep = "*", group = NULL, group_key_sep = "*", unnest_interaction = TRUE, na_rm = TRUE, stat_name = "prop_interact", vartype = "ci", level = 0.95, ...){


  # Get interaction key
  interact_key <- paste(interact, collapse = interact_key_sep)

  # Grouping key
  group_key <- paste(group, collapse = group_key_sep)

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
  # - stat_name: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "{stat_name}" := srvyr::survey_mean(vartype = vartype, level = level, ...),
    "n_unw" := srvyr::unweighted(srvyr::n()))

  # Get unweighted proportions
  to_return <- dplyr::mutate(to_return, "{stat_name}_unw" := prop.table(!!rlang::sym("n_unw")))

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

    if (interact_key != "") {
      # Add group key
      to_return[["interact_key"]] <- interact_key

      # Add group key values
      to_return[["interact_key_value"]] <- do.call(paste, c(to_return[interact], sep = interact_key_sep))

      # Place interact_key in front
      to_return <- dplyr::relocate(to_return, "interact_key", .before = stat_name)
    }
  }

  if (group_key != "") {
    # Add group key
    to_return[["group_key"]] <- group_key

    # Add group key values
    to_return[["group_key_value"]] <- do.call(paste, c(to_return[group], sep = group_key_sep))
    # to_return <- tidyr::unite(to_return, "group_key_value", tidyr::all_of(group), sep = group_key_sep, remove = FALSE)

    # Place group_key in front
    to_return <- dplyr::relocate(to_return, "group_key_value", .before = stat_name)
    to_return <- dplyr::relocate(to_return, "group_key", .before = "group_key_value")
  }


  return(to_return)
}

#' @title Survey proportion
#'
#' @param design A srvyr::design object.
#' @param col A column to calculate proportion from.
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `col` be removed? Default to TRUE.
#' @param stat_name What should the statistic's column be named? Default to "prop".
#' @param ... Other parameters to pass to `srvyr::survey_prop()`.
#'
#' @inheritParams srvyr::survey_prop
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-proportion data frame
#'
#' @export
svy_prop <- function(design, col, group = NULL, na_rm = TRUE, stat_name = "prop", vartype = "ci", level = 0.95, deff = FALSE, ...){

  # Get col name
  col_name <- rlang::as_name(rlang::enquo(col))

  # Get number of NAs
  na_count_tot <- sum(is.na(srvyr::pull(design, {{ col }})))
  n_tot <- nrow(design)

  # if(na_rm & deff & na_count > 0) rlang::warn(c("`na_rm = TRUE` and `deff = TRUE` cannot be used together when NAs exist.",
  #                                                 "*" = glue::glue("There are ", na_count, " NAs in column: ", "{col_name}."),
  #                                                 "i" = "Please rescale weigx²ht or change parameters."))

  # Remove NAs
  if (na_rm) design <- srvyr::drop_na(design, {{ col }})

  # Group design for calculation
  to_return <- srvyr::group_by(design, srvyr::across({{ group }}), srvyr::across({{ col }}))

  # Summarize design
  # - stat_name: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "{stat_name}" := srvyr::survey_prop(vartype = vartype, level = level, deff = deff, ...),
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

  # Add column name
  to_return <- dplyr::rename(to_return, values = {{ col }})

  to_return <- dplyr::mutate(
    to_return,
    name = col_name,
    .before = !!rlang::sym("values"))

  return(to_return)
}

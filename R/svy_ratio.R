#' @title Survey ratio
#'
#' @param design A srvyr::design object
#' @param num The numerator column
#' @param denom The denominator column
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group
#' @param stat_name What should the statistic's column be named? Default to "ratio"
#' @param na_rm Boolean. Remove any line that as an NA in `num` or `denom` Default to TRUE.
#' @param ... Parameters to pass to srvyr::survey_mean()
#'
#' @family survey analysis functions
#'
#' @return A survey-summarized-ratio data frame
#'
#' @export
svy_ratio <- function(design, num, denom, group = NULL, na_rm = TRUE, stat_name = "ratio", ...){

  # Get col name
  num_name <- rlang::as_name(rlang::enquo(num))
  denom_name <- rlang::as_name(rlang::enquo(denom))

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
  # - stat_name: the weighted proportion of obs
  # - n_unw: the unweighted count of obs
  to_return <- srvyr::summarize(
    to_return,
    "{stat_name}" := srvyr::survey_ratio({{ num }}, {{ denom }}, ...),
    "{stat_name}_unw" := srvyr::unweighted(sum({{ num }}) / sum({{ denom }})),
    "n_unw" := srvyr::unweighted(srvyr::n()))

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

  return(to_return)
}



#' @title Kobo survey analysis from a data analysis plan
#'
#' @param design A srvyr::design object.
#' @param dap A well formatted data analysis plan.#'
#'
#' @section Specifics:
#'
#' * It takes the default for most variables, e.g. it tries to label, it calculates confidence intervals, and separators are the default.
#' * Choices cannot be NULL.
#' * The separator for choices can be changed.
#' * Level can be changed.
#'
#'
#' @inheritParams kobo_analysis
#'
#' @importFrom rlang `:=`
#'
#' @family functions for analyzing from Kobo tool
#'
#' @return A survey-summarized-median data frame
#'
#' @export
#'
kobo_analysis_from_dap <- function(design, dap, survey, choices, group = NULL, level = 0.95, choices_sep = "/"){

  #------ Checks

  # Check if all necesarry columns are here
  if_not_in_stop(dap, c("analysis", "var", "na_rm"), "dap")

  # Check if types are the right ones
  analysis_type <- c("mean", "median", "select_multiple", "select_one", "ratio")
  are_in_set(dap, "analysis", c("mean", "median", "select_multiple", "select_one", "ratio"), "Please provide only existing types which are either 'mean', 'median', 'select_multiple', 'select_one' or 'ratio'.")

  if (nrow(dap) == 0) rlang::abort("'dap' does not contain any line. Please provide a non-empty data analysis plan.")

  # Create a id for analysis
  dap[["id_analysis"]] <- paste0("analysis_", row.names(dap))

  # Split to map out the analyses
  split_dap <- impactR.utils::named_group_split(dap, "id_analysis")

  # Map out the analysis
  analysis <- purrr::pmap(
    dplyr::select(dap, dplyr::all_of(c("analysis", "var", "na_rm", "id_analysis"))),
    \(analysis, var, na_rm, id_analysis) {

      if (na_rm == "yes") na_rm_lgl <- TRUE else na_rm_lgl <- FALSE

      # For all analyses, but ratio, it's straightforward
      if (analysis == "ratio") {
        # For ratio, splitting "var" is needed
        # If there is a white space wandering around, remove it
        ratio <- stringr::str_squish(var)
        # Split
        ratio <- stringr::str_split_1(var, ",")
        # Prepare named vector
        var <- ratio[2]
        names(var) <- ratio[1]
      }

      # Run the analysis
      an <- kobo_analysis(design, analysis = analysis, vars = var, survey = survey, choices = choices, group = group, na_rm = na_rm_lgl, level = level, choices_sep = choices_sep)

      an <- dplyr::mutate(an, "id_analysis" := id_analysis)
    }
  )

  # Bind all analyses together
  analysis <- purrr::list_rbind(analysis)

  # Join back the added columns
  analysis <- dplyr::left_join(
    analysis,
    impactR.utils::df_diff(dap, analysis, !!rlang::sym("id_analysis")),
    by = "id_analysis")


  return(analysis)
#

}

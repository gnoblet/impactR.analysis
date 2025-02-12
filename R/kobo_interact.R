#' Get the proportion of the interaction of variables
#'
#' A wrapper around [svy_interact()] to get the proportion of the interaction of variables, and using the Kobo tool to get labels.
#'
#' @param design A srvyr::design object.
#' @param vars Variables to calculate proportion from.
#' @param survey The survey sheet from Kobo (with column "type" split). See `split_survey()`.
#' @param choices The choices sheet from Kobo. If not NULL, the function tries to retrieve labels.
#' @param group A quoted vector of columns to group by. Default to NULL for no group.
#' @param label_survey Boolean. Retrieve var's label from the survey sheet? Default to TRUE.
#'
#' @inheritParams svy_interact
#'
kobo_interact <- function(design, vars, interact_key_sep = " -/- ", survey, choices = NULL, group = NULL, group_key_sep = " -/- ", label_survey = TRUE, na_rm = TRUE, vartype = "ci", level = 0.95){

  #------ Gather arguments

  # Grouping key
  group_key <- paste(vars, collapse = group_key_sep)


  #------ Checks

  # Check if vars are in design
  checkmate::assertSubset(vars, colnames(design))

  # Check survey columns
  checkmate::assertSubset(c("type", "name"), colnames(survey))
  if (!is.null(choices)) checkmate::assertSubset(c("label", "name"), colnames(choices))


  # Calculate proportion of interactions
  interaction <- svy_interact(design = design, interact = vars, interact_key_sep = interact_key_sep, group = group, group_key_sep = group_key_sep, na_rm = na_rm, vartype = vartype, level = level)

  # Type of analysis is select_multiple
  interaction[["analysis"]] <- "interact"

  # Get label for var
  if (label_survey) {
    var_labels <- impactR.kobo::get_survey_labels(survey, !!!vars, output_df = TRUE)
    var_labels <- dplyr::rename(var_labels,
                                "var_label" := !!rlang::sym("label"),
                                "var" := !!rlang::sym("name"))
  }

  if (!is.null(choices)) {
    # Get labels for var values
    var_value_labels <- purrr::map(vars, \(x) {
      lab <- impactR.kobo::get_survey_choices(survey, choices, !!x, label = TRUE)
      lab <- dplyr::mutate(
        lab,
        "label" := as.character(!!rlang::sym("label")),
        "name" := as.character(!!rlang::sym("name")))
    })
    var_value_labels <- purrr::list_rbind(var_value_labels)
    var_value_labels <- dplyr::rename(var_value_labels,
                                      "var" := !!rlang::sym("col"),
                                      "var_value_label" := !!rlang::sym("label"),
                                      "var_value" := !!rlang::sym("name"))
  }

  if (label_survey |!is.null(choices)){
    # Interaction sep
    interaction_sep <- tidyr::separate_longer_delim(interaction, dplyr::all_of(c("interact_key", "interact_key_value")), delim = interact_key_sep)
    interaction_sep <- dplyr::group_by(interaction_sep, !!rlang::sym("analysis_key"))
  }

  if(label_survey){
    # Join
    interaction_sep_var_labels <- dplyr::left_join(interaction_sep, var_labels, by = c("interact_key" = "var"))
    # Interaction sep
    interaction_sep_var_labels <- dplyr::mutate(
      interaction_sep_var_labels,
      "var_label" := paste(!!rlang::sym("var_label"), collapse = " -/- "),
      .keep = "used")
    interaction_sep_var_labels <- dplyr::ungroup(interaction_sep_var_labels)
    interaction_sep_var_labels <- dplyr::distinct(interaction_sep_var_labels)
    # Re-join labels
    interaction <- dplyr::left_join(interaction, interaction_sep_var_labels, by = "analysis_key")
  }

  if (!is.null(choices)) {
    # Join
    interaction_sep_var_value_labels <- dplyr::left_join(interaction_sep, var_value_labels, by = c("interact_key" = "var", "interact_key_value" = "var_value"))
    # Interaction sep
    interaction_sep_var_value_labels <- dplyr::mutate(
      interaction_sep_var_value_labels,
      "var_value_label" := paste(!!rlang::sym("var_value_label"), collapse = " -/- "),
      .keep = "used")
    interaction_sep_var_value_labels <- dplyr::ungroup(interaction_sep_var_value_labels)
    interaction_sep_var_value_labels <- dplyr::distinct(interaction_sep_var_value_labels)
    # Re-join labels
    interaction <- dplyr::left_join(interaction, interaction_sep_var_value_labels, by = "analysis_key")
  }

  return(interaction)

}

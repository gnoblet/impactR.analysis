#------------ Change from get_choices with the addition of parameter sep = "_"
get_choices <- function(survey, choices, col, conc = T, label = F, sep1 = "_", sep2 = ".") {
  col_name <- rlang::as_name(rlang::enquo(col))
  # if_vec_not_in_stop(survey$name, col_name, "survey$name", "col")  to_return <- survey |>    dplyr::filter(.data$name == col_name) |>    dplyr::select(.data$list_name)

  if (nrow(to_return) == 0) {
    rlang::warn(glue::glue("Col: '{col_name}' is not in survey$name.", "An empty vector or an empty tibble is returned.", .sep = "\n"))
    if (label) {
      return(tibble::tibble(
        name = character(),
        label = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  # If there are more than one row, throw a warning but continue keeping the 1st row  if (nrow(to_return) > 1) {
  rlang::warn(glue::glue(sep = "\n", "There are more than one line in the survey sheet for col '{col_name}'.", "The head was sliced to go on, but please check."))

  to_return <- to_return |> dplyr::slice_head(n = 1)

  if (is.na(dplyr::pull(to_return, .data$list_name))) {
    rlang::warn(glue::glue("There is no list_name listed in survey for col: '{col_name}'.", "An empty vector or an empty tibble is returned, please check.", .sep = "\n"))
    if (label) {
      return(tibble::tibble(
        name = character(),
        label = character()
      ))
    } else if (!label) {
      return(character())
    }
  }
  if (length(subvec_in(dplyr::pull(to_return, .data$list_name), choices$list_name)) == 0) {
    rlang::warn(glue::glue("There is no corresponding list_name in choices for col: '{col_name}'.", "An empty vector or an empty tibble is returned.", .sep = "\n"))
    if (label) {
      return(tibble::tibble(
        name = character(),
        label = character()
      ))
    } else if (!label) {
      return(character())
    }
  }

  to_return <- to_return |> dplyr::left_join(choices, by = "list_name")

  if (!label) {
    to_return <- to_return |> dplyr::pull(.data$name)
    if (rlang::is_true(conc)) {
      to_return <- tibble::tibble(sep1 = stringr::str_c(col_name, to_return, sep = sep1), sep2 = stringr::str_c(col_name, to_return, sep = sep2))
    }
  } else {
    to_return <- to_return |> dplyr::select(.data$name, .data$label)
  }
  return(to_return)
}

# Get multiple choice main column names from the survey sheetmultiple_choice <- impactR::get_select_multiple(survey)
# Multiple choice column names as a tibble with both with sep = "_" et sep = "."
multiple_choice_child <- purrr::map(
  multiple_choice,
  \(x) get_choices(survey, choices, {{ x }})) |>
  purrr::set_names(multiple_choice) |>
  dplyr::bind_rows(.id = "parent")
# Only keep columns that are in the datasetmultiple_choice_child <- multiple_choice_child |>  dplyr::filter(sep1 %in% colnames(main))
# Renames columnsmain <- rename_cols(main, old_cols = multiple_choice_child$sep1, new_cols = multiple_choice_child$sep2)
# Rename columns

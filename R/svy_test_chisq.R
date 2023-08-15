#' @title Chi-squared survey tests, wrapper around survey::svychisq()
#'
#' @param design A srvyr::design object.
#' @param group A quoted or unquoted vector of columns to group by. Default to NULL for no group.
#' @param na_rm Should NAs from `var1` and `var2` be removed? Default to TRUE.
#' @param var1 Var name 1.
#' @param var2 Var name 2.
#' @param ... Other parameters to pass to `survey::svychisq()`.
#'
#' @family test functions
#'
#' @inheritParams survey::svychisq
#'
#' @return A data frame of the test output
#'
#' @export
svy_test_chisq <- function(design, var1, var2, group = NULL, na_rm = TRUE, statistic = "F", ...){

  # Get column names
  var1_name <- rlang::as_name(rlang::enquo(var1))
  var2_name <- rlang::as_name(rlang::enquo(var2))

  if (na_rm) {
    design <- srvyr::drop_na(design, {{ var1 }})
    design <- srvyr::drop_na(design, {{ var2 }})
  }

  formula <- stats::as.formula(paste("~", var1_name, "+", var2_name))

  to_return <- srvyr::svychisq(
    formula,
    design,
    statistic = statistic,
    na.rm = FALSE, ...)

  # Outputs of every statistic is not the same
  boolean_statistic_df <- statistic %in% c("Chisq", "lincom", "saddlepoint")
  boolean_statistic_value <- statistic %in% c("lincom", "saddlepoint")

  # Construct the output
  to_return <- data.frame(
    statistic = statistic[1],
    value = if (boolean_statistic_value) NA_real_ else to_return[["statistic"]],
    p_value = to_return[["p.value"]],
    ndf = if (boolean_statistic_df) NA_real_ else to_return[["parameter"]][["ndf"]],
    ddf = if (boolean_statistic_df) NA_real_ else to_return[["parameter"]][["ddf"]],
    df = if (!(statistic %in% "Chisq")) NA_real_ else to_return[["parameter"]],
    method = to_return[["method"]]
  )

  return(to_return)
}



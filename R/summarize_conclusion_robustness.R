#' Calculate re-analysis inferential robustness
#' 
#' The function calculates the inferential robustness for a given threshold across a grouping variable.
summarize_conclusion_robustness <- function(data, grouping_var, threshold = 100, operator = "==") {
  threshold_fraction <- threshold / 100
  # Custom function to make robustness comparison dynamic
  comparison_fn <- get_comparison_fn(operator)
  
  if (missing(grouping_var)) {
    data |> 
      dplyr::group_by(simplified_paper_id) |> 
      dplyr::summarise(
        same_conclusion_fraction = mean(task1_categorisation_plotting == "Same conclusion"),
        robust = dplyr::if_else(comparison_fn(same_conclusion_fraction, threshold_fraction), "Inferentially robust", "Inferentially not robust"),
        robust = factor(robust, levels = c("Inferentially robust", "Inferentially not robust"))
      ) |> 
      dplyr::ungroup() |> 
      calculate_percentage(robust) |> 
      dplyr::ungroup()
  } else {
    data |> 
      dplyr::group_by(simplified_paper_id, {{grouping_var}}) |> 
      dplyr::summarise(
        same_conclusion_fraction = mean(task1_categorisation_plotting == "Same conclusion"),
        robust = dplyr::if_else(comparison_fn(same_conclusion_fraction, threshold_fraction), "Inferentially robust", "Inferentially not robust"),
        robust = factor(robust, levels = c("Inferentially robust", "Inferentially not robust"))
      ) |> 
      dplyr::ungroup() |> 
      dplyr::count({{grouping_var}}, robust) |> 
      dplyr::group_by({{grouping_var}}) |> 
      add_percentage(n) |>
      dplyr::ungroup()
  }
}

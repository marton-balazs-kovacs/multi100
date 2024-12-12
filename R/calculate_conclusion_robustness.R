#' independent of calculate_conclusion function
calculate_conclusion_robustness <- function(data, grouping_var, categorization_var, threshold = 100, operator = "==") {
  threshold_fraction <- threshold / 100
  # Custom function to make robustness comparison dynamic
  comparison_fn <- get_comparison_fn(operator)
  
  if (missing(grouping_var)) {
    data %>%
      dplyr::group_by(simplified_paper_id) %>% 
      dplyr::summarise(
        same_conclusion_fraction = mean({{categorization_var}} == "Same conclusion"),
        robust = dplyr::if_else(comparison_fn(same_conclusion_fraction, threshold_fraction), "Inferentially robust", "Inferentially not robust"),
        robust = factor(robust, levels = c("Inferentially robust", "Inferentially not robust"))
      ) |> 
      dplyr::ungroup() %>%
      dplyr::count(robust) %>%
      dplyr::mutate(
        N = sum(n),
        relative_frequency = n / N,
        percentage = round(relative_frequency * 100, 2)
      ) |>
      dplyr::ungroup()
  } else {
    data %>% 
      dplyr::group_by(paper_id, {{grouping_var}}) %>%
      dplyr::summarise(
        same_conclusion_fraction = mean({{categorization_var}} == "Same conclusion"),
        robust = dplyr::if_else(comparison_fn(same_conclusion_fraction, threshold_fraction), "Inferentially robust", "Inferentially not robust"),
        robust = factor(robust, levels = c("Inferentially robust", "Inferentially not robust"))
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::count({{grouping_var}}, robust) %>% 
      dplyr::group_by({{grouping_var}}) %>% 
      dplyr::mutate(
        N = sum(n),
        relative_frequency = n / N,
        percentage = round(relative_frequency * 100, 2)
      ) |> 
      dplyr::ungroup()
  }
}

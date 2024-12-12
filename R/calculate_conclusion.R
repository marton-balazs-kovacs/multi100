#' Prepare dataset for plotting the proportion of conclusions
calculate_conclusion <- function(data, grouping_var, categorization_var) {
  data %>% 
    dplyr::select({{ grouping_var }}, {{ categorization_var }}) %>% 
    dplyr::count({{ grouping_var }}, {{ categorization_var }}) %>% 
    dplyr::group_by({{ grouping_var }}) %>%
    tidyr::complete({{ categorization_var }}, fill = list(n = 0)) %>% 
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = round(relative_frequency * 100),
    ) %>%
    ungroup()
}
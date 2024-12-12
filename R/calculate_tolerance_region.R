#' Prepare dataset for tolerance region plot
calculate_tolerance_region <- function(data, grouping_var, drop_missing = FALSE, weight = NULL) {
  res <-
    data %>%
    # Drop missing original_cohens_d
    dplyr::filter(!is.na(original_cohens_d)) %>% 
    dplyr::select({{ grouping_var }}, reanalysis_cohens_d, original_cohens_d) |> 
    dplyr::mutate(
      threshold = if (!is.null(weight)) original_cohens_d * weight else 0.05,
      tolarence_region_lower = original_cohens_d - threshold,
      tolarence_region_upper = original_cohens_d + threshold,
      {{ grouping_var }} := as.factor({{ grouping_var }}),
      is_within_region = dplyr::case_when(
        reanalysis_cohens_d >= tolarence_region_lower & reanalysis_cohens_d <= tolarence_region_upper ~ "Within tolerance region",
        reanalysis_cohens_d < tolarence_region_lower | reanalysis_cohens_d > tolarence_region_upper ~ "Outside of tolerance region",
        is.na(reanalysis_cohens_d) ~ "Missing"
      ),
      is_within_region = factor(is_within_region, levels = c("Within tolerance region", "Outside of tolerance region", "Missing"))
    ) %>% 
    dplyr::count({{ grouping_var }}, is_within_region) %>% 
    dplyr::group_by({{ grouping_var }}) %>%
    tidyr::complete(is_within_region, fill = list(n = 0))
  
  if (drop_missing) {
    res <-
      res %>% 
      dplyr::filter(is_within_region != "Missing")
  }
  
  res <-
    res %>% 
    dplyr::group_by({{ grouping_var }}) %>% 
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = relative_frequency * 100
    ) %>% 
    dplyr::ungroup()
  
  res
}
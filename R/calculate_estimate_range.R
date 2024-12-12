#' Prepare dataset for effect size robustness plot
calculate_estimate_range <- function(data, grouping_var) {
  data %>% 
    dplyr::select(paper_id, {{grouping_var}}, reanalysis_cohens_d) %>% 
    dplyr::group_by(paper_id, {{grouping_var}}) %>% 
    # dplyr::filter(reanalysis_cohens_d == min(reanalysis_cohens_d, na.rm = FALSE) | reanalysis_cohens_d == max(reanalysis_cohens_d, na.rm = FALSE)) %>% 
    dplyr::summarise(
      estimate_range = max(reanalysis_cohens_d, na.rm = TRUE) - min(reanalysis_cohens_d, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
}
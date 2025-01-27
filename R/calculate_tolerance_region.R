#' Check whether the re-analysis effect sizes are within the tolerance region
calculate_tolerance_region <- function(data, threshold = NULL, weight = NULL) {
  # Validate input: only one of threshold or weight should be provided
  if (!is.null(threshold) && !is.null(weight)) {
    stop("Only one of 'threshold' or 'weight' can be provided, not both.")
  }
  # Validate input: at least threshold or weight is provided
  if (is.null(threshold) && is.null(weight)) {
    stop("Provide either 'threshold' or 'weight'.")
  }
  
  data %>%
    # Drop missing original_cohens_d
    dplyr::filter(!is.na(original_cohens_d)) %>% 
    dplyr::mutate(
      # Calculate tolerance region
      threshold = if (!is.null(weight)) original_cohens_d * weight else threshold,
      tolarence_region_lower = original_cohens_d - threshold,
      tolarence_region_upper = original_cohens_d + threshold,
      # Categorize re-analysis effect sizes
      is_within_region = dplyr::case_when(
        reanalysis_cohens_d >= tolarence_region_lower & reanalysis_cohens_d <= tolarence_region_upper ~ "Within tolerance region",
        reanalysis_cohens_d < tolarence_region_lower | reanalysis_cohens_d > tolarence_region_upper ~ "Outside of tolerance region",
        is.na(reanalysis_cohens_d) ~ "Missing"
      ),
      is_within_region = factor(is_within_region, levels = c("Within tolerance region", "Outside of tolerance region", "Missing"))
    )
}

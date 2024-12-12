#' Function to calculate proportions within tolerance regions for a given threshold
calculate_tolerance_region_proportions <- function(data, threshold = NULL, weight = NULL) {
  region_data <-
    data  |> 
    dplyr::select(simplified_paper_id, original_cohens_d, reanalysis_cohens_d) |> 
    dplyr::filter(!is.na(original_cohens_d)) |> 
    dplyr::filter(!is.na(reanalysis_cohens_d)) |>
    dplyr::mutate(
      threshold = if (!is.null(weight)) original_cohens_d * weight else threshold,
      tolerance_region_lower = original_cohens_d - threshold,
      tolerance_region_upper = original_cohens_d + threshold,
      is_within_region = case_when(
        reanalysis_cohens_d >= tolerance_region_lower & reanalysis_cohens_d <= tolerance_region_upper ~ "Within tolerance region",
        reanalysis_cohens_d < tolerance_region_lower | reanalysis_cohens_d > tolerance_region_upper~ "Outside of tolerance region",
        is.na(reanalysis_cohens_d) ~ "Missing"
      ),
      is_within_region = as.factor(is_within_region)
    )
  
  analysis_propotion <-
    region_data |> 
    dplyr::count(is_within_region) %>%
    tidyr::complete(is_within_region, fill = list(n = 0)) |> 
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      analysis_percentage = round(relative_frequency * 100, 2)
    ) %>%
    dplyr::filter(is_within_region == "Within tolerance region") |> 
    dplyr::select(analysis_percentage)
  
  paper_proportion <-
    region_data |> 
    dplyr::count(simplified_paper_id, is_within_region) %>% 
    dplyr::group_by(simplified_paper_id) %>%
    tidyr::complete(is_within_region, fill = list(n = 0)) |> 
    dplyr::group_by(simplified_paper_id) %>% 
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = relative_frequency * 100
    ) %>% 
    dplyr::summarise(
      robust = if_else(any(is_within_region == "Within tolerance region" & relative_frequency == 1), "Inferentially robust", "Inferentially not Robust"),
      robust = factor(robust, levels = c("Inferentially robust", "Inferentially not Robust"))
    ) |> 
    dplyr::ungroup() |> 
    dplyr::count(robust) |> 
    tidyr::complete(robust, fill = list(n = 0)) |> 
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      paper_percentage = round(relative_frequency * 100, 2)
    ) |> 
    dplyr::filter(robust == "Inferentially robust") |> 
    dplyr::select(paper_percentage)
  
  if (!is.null(weight) & is.null(threshold)) {
    dplyr::bind_cols(analysis_propotion, paper_proportion) |> 
      dplyr::mutate(weight = weight)
  } else if (is.null(weight) & !is.null(threshold)) {
    dplyr::bind_cols(analysis_propotion, paper_proportion) |> 
      dplyr::mutate(threshold = threshold)
  } else (
    stop("Only pass either threshold or weight as an input parameter.")
  )
}
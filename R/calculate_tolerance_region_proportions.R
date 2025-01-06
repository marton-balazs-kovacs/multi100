#' Function to calculate proportions within tolerance regions for a given threshold
calculate_tolerance_region_proportions <- function(data, threshold = NULL, weight = NULL) {
  # Prepare the dataset by calculating the tolerance regions
  region_data <-
    data  |> 
    dplyr::select(simplified_paper_id, original_cohens_d, reanalysis_cohens_d) |> 
    dplyr::filter(!is.na(reanalysis_cohens_d)) |>
    calculate_tolerance_region(threshold = threshold, weight = weight) |> 
    dplyr::filter(is_within_region != "Missing")
  
  # Calculate the proportion of re-analysis effect sizes within the tolerance region on the analysis level
  analysis_propotion <-
    region_data |> 
    dplyr::count(is_within_region) %>%
    tidyr::complete(is_within_region, fill = list(n = 0)) |> 
    add_percentage(n) |> 
    dplyr::rename(analysis_percentage = percentage) |> 
    dplyr::filter(is_within_region == "Within tolerance region") |> 
    dplyr::select(analysis_percentage)
  
  # Summarize estimate robustness on the paper level
  paper_proportion <-
    region_data |> 
    # Calculate the proportion of re-analysis effect sizes within the tolerance region on the paper level
    dplyr::count(simplified_paper_id, is_within_region) %>% 
    dplyr::group_by(simplified_paper_id) %>%
    tidyr::complete(is_within_region, fill = list(n = 0)) |> 
    dplyr::group_by(simplified_paper_id) %>% 
    add_percentage(n) |> 
    dplyr::summarise(
      robust = if_else(any(is_within_region == "Within tolerance region" & freq == 1), "Inferentially robust", "Inferentially not Robust"),
      robust = factor(robust, levels = c("Inferentially robust", "Inferentially not Robust"))
    ) |> 
    dplyr::ungroup() |> 
    dplyr::count(robust) |> 
    tidyr::complete(robust, fill = list(n = 0)) |> 
    add_percentage(n) |> 
    dplyr::rename(paper_percentage = percentage) |> 
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

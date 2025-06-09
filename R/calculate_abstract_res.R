calculate_abstract_res <- function(data) {
  # On the individual analysis level
  # +-0.05 threshold for tolerance region
  analysis_level_tolerance_region <-
    data |>
    dplyr::select(paper_id,
                  analyst_id,
                  original_cohens_d,
                  reanalysis_cohens_d) |>
    calculate_tolerance_region(threshold = 0.05) |>
    # Exclude missing re-analysis effect sizes
    dplyr::filter(is_within_region != "Missing") |>
    calculate_percentage(is_within_region)
  
  # Calculate value for abstract
  analysis_level_within_tolerance_region_5 <-
    dplyr::filter(analysis_level_tolerance_region,
                  is_within_region == "Within tolerance region")
  
  # +-0.2 threshold for tolerance region
  analysis_level_tolerance_region_20 <-
    data |>
    dplyr::select(paper_id,
                  analyst_id,
                  original_cohens_d,
                  reanalysis_cohens_d) |>
    calculate_tolerance_region(threshold = 0.2) |>
    # Exclude missing re-analysis effect sizes
    dplyr::filter(is_within_region != "Missing") |>
    calculate_percentage(is_within_region)
  
  # Calculate value for abstract
  analysis_level_within_tolerance_region_20 <-
    dplyr::filter(analysis_level_tolerance_region_20,
                  is_within_region == "Within tolerance region")
  
  # Analysis level conclusion
  analysis_level_conclusion <-
    data |> 
    calculate_conclusion()
  
  # Return results
  return(
    list(
      analysis_level_within_tolerance_region_5 = analysis_level_within_tolerance_region_5,
      analysis_level_within_tolerance_region_20 = analysis_level_within_tolerance_region_20,
      analysis_level_conclusion = analysis_level_conclusion
    )
  )
}

#' Prepare dataset for tolerance region plot
summarize_tolerance_region <- function(data, grouping_var, drop_missing = FALSE, threshold = NULL, weight = NULL) {
  res <-
    data |> 
    dplyr::select({{ grouping_var }}, reanalysis_cohens_d, original_cohens_d) |> 
    dplyr::mutate(
      {{ grouping_var }} := as.factor({{ grouping_var }})
    ) |> 
    calculate_tolerance_region(threshold = threshold, weight = weight) |> 
    dplyr::count({{ grouping_var }}, is_within_region) |> 
    dplyr::group_by({{ grouping_var }}) |> 
    tidyr::complete(is_within_region, fill = list(n = 0))
  
  if (drop_missing) {
    res <-
      res |> 
      dplyr::filter(is_within_region != "Missing")
  }
  
  res <-
    res %>%
    dplyr::group_by({{ grouping_var }}) |>
    add_percentage(n) |>
    dplyr::ungroup()
  
  res
}

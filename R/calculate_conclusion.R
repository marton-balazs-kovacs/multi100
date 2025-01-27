#' Prepare dataset for plotting the proportion of conclusions
calculate_conclusion <- function(data, grouping_var) {
  data |> 
    dplyr::rename(categorisation = task1_categorisation_plotting) |>
    dplyr::mutate(
      categorisation = forcats::fct_relevel(categorisation, c("Same conclusion", "No effect/inconclusive", "Opposite effect"))
    ) |> 
    dplyr::select({{ grouping_var }}, categorisation) |> 
    dplyr::count({{ grouping_var }}, categorisation) |> 
    dplyr::group_by({{ grouping_var }}) |> 
    tidyr::complete(categorisation, fill = list(n = 0)) |> 
    add_percentage(n) |> 
    ungroup()
}

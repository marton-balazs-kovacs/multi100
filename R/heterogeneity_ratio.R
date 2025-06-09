heterogeneity_ratio <- function(data, grouping_var) {
  data |>
    dplyr::mutate(reanalysis_se_d = se_d(r = reanalysis_correlation_coef, n = reanalysis_model_sample_size)) |>
    dplyr::group_by({{grouping_var}}) |>
    dplyr::summarise(
      paper_reanalysis_sd = sd(reanalysis_cohens_d),
      average_reanalysis_se = mean(reanalysis_se_d),
      heterogeneity_ratio = paper_reanalysis_sd / average_reanalysis_se
    )
}

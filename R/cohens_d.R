#' Calculate Cohen's d
cohens_d <- function(r, type_of_statistic, test_statistic, sample_size) {
  dplyr::case_when(
    type_of_statistic %in% c("z", "chi2", "t", "F", "r", "tau") & !is.na(r) ~ round(sign(r)*sqrt(4/((1/r^2)-1)), 3),
    type_of_statistic == "reg_coeff" ~ test_statistic/sqrt(sample_size),
    TRUE ~ NA_real_
  )
}

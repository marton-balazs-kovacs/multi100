#' Prepare dataset for tolarence region plot
library(dplyr)

calculate_tolarence_region <- function(data, grouping_var) {
  data %>%
    # TODO: this simulation will be replaced if the effect sizes are calculated
    # variable names might change!
    mutate(r_analyst = runif(508, min = 0.01, max = 0.99)) %>%
    group_by(Paper_ID) %>% 
    mutate(r_original = runif(1, min = 0.01, max = 0.99)) %>% 
    ungroup() %>% 
    # beginning of the actual function
    select({{ grouping_var }}, r_analyst, r_original) %>% 
    mutate(
      tolarence_region_lower = r_original - 0.05,
      tolarence_region_upper = r_original + 0.05,
      {{ grouping_var }} := as.factor({{ grouping_var }})
    ) %>% 
    group_by({{ grouping_var }}) %>%
    mutate(
      is_within_region = case_when(
        r_analyst <= tolarence_region_lower | r_analyst >= tolarence_region_upper ~ "No",
        r_analyst >= tolarence_region_lower | r_analyst <= tolarence_region_upper ~ "Yes"
      )
    ) %>% 
    count({{ grouping_var }}, is_within_region) %>% 
    group_by({{ grouping_var }}) %>% 
    mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = relative_frequency * 100
    ) %>% 
    ungroup()
}

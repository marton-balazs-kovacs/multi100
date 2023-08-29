#' Prepare dataset for tolerance region plot
library(dplyr)

calculate_tolerance_region <- function(data, grouping_var) {
  data %>%
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

#' Create tolarence region plot
plot_tolarence_region <- function(data, grouping_var) {
  data %>% 
    ggplot() +
    aes(
      x = percentage,
      y = {{grouping_var}},
      fill = is_within_region
    ) +
    geom_bar(stat = "identity") +
    scale_x_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 1)) +
    labs(
      x = "Percentage",
      fill = "Within region?"
    ) +
    theme(
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      # axis.text.y=element_text(margin = margin(1, unit = "cm"), vjust =1.5)
    )
}

#' Prepare dataset for effect size robustness plot
calculate_robustness <- function(data, grouping_var) {
  data %>% 
    select(Paper_ID, {{grouping_var}}, r_analyst) %>% 
    group_by(Paper_ID, {{grouping_var}}) %>% 
    filter(r_analyst == min(r_analyst) | r_analyst == max(r_analyst)) %>% 
    summarise(
      robustness = max(r_analyst) - min(r_analyst)
    ) %>% 
    ungroup()
}

#' Create effect size robustness plot
plot_robustness <- function(data, grouping_var) {
  data %>% 
    ggplot() +
    aes(
      x = {{grouping_var}},
      y = robustness
    ) +
    geom_jitter(width = 0.1) +
    labs(
      x = "Discipline",
      y = "Robustness",
    ) +
    theme(
      axis.ticks = element_blank(),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, "pt"),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      # axis.text.y=element_text(margin = margin(1, unit = "cm"), vjust =1.5)
    )
}

#' Prepare dataset for plotting the proportion of conclusions
calculate_conclusion <- function(data, grouping_variable) {
  data %>% 
    select({{ grouping_variable }}, Direction_of_Result) %>% 
    count({{ grouping_variable }}, Direction_of_Result) %>% 
    ungroup() %>% 
    mutate(
      Direction_of_Result = forcats::fct_relevel(Direction_of_Result, c("Same as claimed by the original study", "Opposite as claimed by the original study")),
    ) %>%
    group_by({{ grouping_variable }}) %>% 
    mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = relative_frequency * 100,
    ) %>% 
    ungroup()
}

#' Plot conclusions
plot_conclusion <- function(data, grouping_variable) {
  data %>% 
    ggplot() +
    aes(
      y = {{ grouping_variable }},
      x = percentage,
      fill = Direction_of_Result
    ) +
    geom_bar(
      stat = "identity",
      width = 0.8
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 1)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
      fill = "Direction of\nthe results",
      x = "Percentage"
    ) +
    theme(
      axis.ticks = element_blank(),
      axis.title.y = element_blank(),
      legend.box = "horizontal",
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      legend.position = "bottom",
      panel.background = element_blank(),
      panel.grid = element_blank()
    )
}

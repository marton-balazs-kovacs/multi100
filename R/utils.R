#' Prepare dataset for tolerance region plot
calculate_tolerance_region <- function(data, grouping_var) {
  data %>%
    select({{ grouping_var }}, cohen_analyst, cohen_original) %>% 
    mutate(
      tolarence_region_lower = cohen_original - 0.05,
      tolarence_region_upper = cohen_original + 0.05,
      {{ grouping_var }} := as.factor({{ grouping_var }})
    ) %>% 
    group_by({{ grouping_var }}) %>%
    mutate(
      is_within_region = case_when(
        cohen_analyst <= tolarence_region_lower | cohen_analyst >= tolarence_region_upper ~ "No",
        cohen_analyst >= tolarence_region_lower | cohen_analyst <= tolarence_region_upper ~ "Yes"
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

#' Function to NOT include a vector in another vector
`%ni%` <- Negate(`%in%`)

#' Create tolarence region plot
plot_tolarence_region <- function(data, grouping_var, with_labels = FALSE) {
  plot <-
    data %>% 
    ggplot() +
    aes(
      x = percentage,
      y = {{grouping_var}},
      fill = is_within_region
    ) +
    geom_bar(
      stat = "identity",
      color = "black"
      ) +
    scale_x_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 1)) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
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
  
  if (with_labels) {
    plot <-
      plot +
      geom_text(
        data = . %>% filter(percentage > 10),
        aes(label = scales::percent(round(percentage), scale = 1)),
        position = position_stack(vjust = 0.5),  # Adjust the vertical position of labels
        color = "white",  # Label text color
        size = 5  # Label text size
      )
  }
  
  return(plot)
}

#' Prepare dataset for effect size robustness plot
calculate_robustness <- function(data, grouping_var) {
  data %>% 
    select(paper_id, {{grouping_var}}, cohen_analyst) %>% 
    group_by(paper_id, {{grouping_var}}) %>% 
    filter(cohen_analyst == min(cohen_analyst) | cohen_analyst == max(cohen_analyst)) %>% 
    summarise(
      robustness = max(cohen_analyst) - min(cohen_analyst)
    ) %>% 
    ungroup()
}

#' Create effect size robustness plot
plot_robustness <- function(data, grouping_var, xlab = "") {
  data %>% 
    ggplot() +
    aes(
      x = {{grouping_var}},
      y = robustness
    ) +
    geom_jitter(width = 0.1) +
    labs(
      x = xlab,
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
calculate_conclusion <- function(data, grouping_variable, categorization_variable) {
  data %>% 
    select({{ grouping_variable }}, {{ categorization_variable }}) %>% 
    count({{ grouping_variable }}, {{ categorization_variable }}) %>% 
    group_by({{ grouping_variable }}) %>%
    complete({{ categorization_variable }}, fill = list(n = 0)) %>% 
    mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = round(relative_frequency * 100),
    ) %>%
    ungroup()
}

#' Plot conclusions
plot_conclusion <- function(data, grouping_variable, categorization_variable, with_labels = FALSE, y_lab = NULL) {
  plot <- 
    data %>% 
    ggplot() +
    aes(
      y = {{ grouping_variable }},
      x = percentage,
      fill = {{ categorization_variable }}
    ) +
    geom_bar(
      color = "black",
      stat = "identity",
      width = 0.8) +
    scale_x_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 1)) +
    scale_y_discrete(expand = c(0, 0)) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
    labs(
      fill = str_to_sentence(quo_name(enquo(categorization_variable))),
      x = "Percentage",
      y = y_lab
    ) +
    theme(
      axis.ticks = element_blank(),
      # axis.title.y = element_blank(),
      # legend.box = "horizontal",
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      # legend.position = "bottom",
      panel.background = element_blank(),
      panel.grid = element_blank()
    )
  
  if (with_labels) {
    plot <-
      plot +
      geom_text(
        data = . %>% filter(percentage > 10),
        aes(label = scales::percent(round(percentage), scale = 1)),
        position = position_stack(vjust = 0.5),  # Adjust the vertical position of labels
        color = "white",  # Label text color
        size = 5  # Label text size
      )
  }
    
    return(plot)
}
#' for now input must be calculate_conclusion() result data
calculate_conclusion_robustness <- function(data, grouping_variable, categorization_variable) {
  if (missing(grouping_variable)) {
    data %>%
      group_by(paper_id) %>% 
      summarise(
        robust = if_else(all({{categorization_variable}} == "Same conclusion"), "Robust", "Not Robust")
      ) %>%
      ungroup() %>% 
      count(robust) %>%
      mutate(
        N = sum(n),
        relative_frequency = n / N,
        percentage = round(relative_frequency * 100, 2)
      )
  } else {
    data %>% 
      group_by(paper_id, {{grouping_variable}}) %>%
      summarise(
        robust = if_else(all({{categorization_variable}} == "Same conclusion"), "Robust", "Not Robust")
      ) %>% 
      ungroup() %>% 
      count({{grouping_variable}}, robust) %>% 
      group_by({{grouping_variable}}) %>% 
      mutate(
        N = sum(n),
        relative_frequency = n / N,
        percentage = round(relative_frequency * 100, 2)
      )
  }
}

plot_conclusion_robustness <- function(data, response_variable, grouping_variable = NULL) {
  plot <-
    data %>% 
    ggplot() +
    aes(
      x = {{response_variable}},
      y = percentage,
      fill = {{grouping_variable}}
        ) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    labs(
      x = "Robustness of the Conclusions",
      y = "Percentage"
    ) +
    theme(
      axis.ticks = element_blank(),
      axis.line = element_line(color = "black"),
      # legend.box = "horizontal",
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      # legend.position = "bottom",
      panel.background = element_blank(),
      panel.grid = element_blank()
    )
  
  if (!rlang::quo_is_missing(rlang::enquo(grouping_variable))) {
    legend_label <- str_to_sentence(str_replace_all(quo_name(enquo(grouping_variable)), "_", " "))
    
    plot <-
      plot +
      labs(fill = legend_label) +
      theme(
        legend.position = "bottom"
      )
  }
  
  plot
}

#' Function to check different response to a question in case of multiple analysis
check_diff_response <- function(data, id_variable, response_variable) {
  data %>% 
    count({{id_variable}}, {{response_variable}}) %>% 
    group_by({{id_variable}}) %>% 
    mutate(n_per_analyst = n()) %>% 
    arrange(desc(n_per_analyst)) %>%
    filter(n_per_analyst > 1L)
  
    # distinct({{id_variable}}) %>% 
    # pull({{id_variable}})
}

keep_first_response <- function(data, id_var, time_var) {
  data %>%
    group_by({{id_var}}) %>% 
    mutate(
      first_response = if_else(
        {{time_var}} == min({{time_var}}),
        TRUE, FALSE)
    ) %>% 
    ungroup() %>% 
    filter(first_response)
}

#' Function to calculate the count and percentage of the responses for a response variable
calculate_percentage <- function(data, response_var) {
  data %>% 
    count({{response_var}}) %>% 
    ungroup() %>% 
    mutate(
      N = sum(n),
      freq = n / N,
      percentage = round(freq * 100, 2)
    ) 
}

#' Calculate standard correlation coefficient
standard_correlation_coefficient <- function(type_of_statistic, test_statistic, sample_size, df1, df2) {
  if_else(type_of_statistic == "z",
    tanh(abs(test_statistic)*sqrt(1/(sample_size-3)))*sign(test_statistic),
    if_else(type_of_statistic == "chi2",
            tanh(sqrt(abs(test_statistic))*sqrt(1/(sample_size-3)))*sign(test_statistic),
            if_else(type_of_statistic == "t",
                    sqrt(abs(test_statistic)^2/(test_statistic^2+df1))*sign(test_statistic),
                    if_else(type_of_statistic == "F",
                            sqrt(abs(sqrt(test_statistic))^2/(sqrt(test_statistic)^2+df2))*sign(test_statistic),
                            NA_real_)))
  )
}

#' Calculate Cohen's d
cohens_d <- function(r) {
  sign(r)*sqrt(4/((1/r^2)-1))
}

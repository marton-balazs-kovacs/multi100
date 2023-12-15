#' Prepare dataset for tolerance region plot
calculate_tolerance_region <- function(data, grouping_var, drop_missing = FALSE) {
  res <-
    data %>%
    # Drop missing original_cohens_d
    dplyr::filter(!is.na(original_cohens_d)) %>% 
    dplyr::select({{ grouping_var }}, reanalysis_cohens_d, original_cohens_d) %>% 
    dplyr::mutate(
      tolarence_region_lower = original_cohens_d - 0.05,
      tolarence_region_upper = original_cohens_d + 0.05,
      {{ grouping_var }} := as.factor({{ grouping_var }})
    ) %>% 
    dplyr::group_by({{ grouping_var }}) %>%
    dplyr::mutate(
      is_within_region = dplyr::case_when(
        reanalysis_cohens_d <= tolarence_region_lower | reanalysis_cohens_d >= tolarence_region_upper ~ "No",
        reanalysis_cohens_d >= tolarence_region_lower | reanalysis_cohens_d <= tolarence_region_upper ~ "Yes",
        is.na(reanalysis_cohens_d) ~ "Missing"
      ),
      is_within_region = as.factor(is_within_region)
    ) %>% 
    dplyr::count({{ grouping_var }}, is_within_region) %>% 
    dplyr::group_by({{ grouping_var }}) %>%
    tidyr::complete(is_within_region, fill = list(n = 0))
    
    if (drop_missing) {
      res <-
        res %>% 
        dplyr::filter(is_within_region != "Missing")
    }
  
  res <-
    res %>% 
    dplyr::group_by({{ grouping_var }}) %>% 
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = relative_frequency * 100
    ) %>% 
    dplyr::ungroup()
  
  res
}

#' Function to NOT include a vector in another vector
`%ni%` <- Negate(`%in%`)

#' Create tolarence region plot
plot_tolarence_region <- function(data, grouping_var, with_labels = FALSE, ylab = NULL) {
  plot <-
    data %>% 
    # mutate(
    #   {{grouping_var}} := as.factor({{grouping_var}}),
    #   {{grouping_var}} := fct_reorder({{grouping_var}}, percentage)
    # ) %>% 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = percentage,
      y = {{grouping_var}},
      fill = is_within_region
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      color = "black"
      ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 1)) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
    ggplot2::labs(
      x = "Percentage",
      y = ylab,
      fill = "Within region"
    ) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      # axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      # axis.text.y=element_text(margin = margin(1, unit = "cm"), vjust =1.5)
    )
  
  if (with_labels) {
    plot <-
      plot +
      ggplot2::geom_text(
        data = . %>% dplyr::filter(percentage > 10),
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
    dplyr::select(paper_id, {{grouping_var}}, reanalysis_cohens_d) %>% 
    dplyr::group_by(paper_id, {{grouping_var}}) %>% 
    dplyr::filter(reanalysis_cohens_d == min(reanalysis_cohens_d) | reanalysis_cohens_d == max(reanalysis_cohens_d)) %>% 
    dplyr::summarise(
      robustness = max(reanalysis_cohens_d) - min(reanalysis_cohens_d)
    ) %>% 
    dplyr::ungroup()
}

#' Create effect size robustness plot
plot_robustness <- function(data, grouping_var, xlab = "") {
  data %>% 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = {{grouping_var}},
      y = robustness
    ) +
    ggplot2::geom_jitter(width = 0.1) +
    ggplot2::labs(
      x = xlab,
      y = "Robustness in Cohen's d",
    ) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 20, r = 20, b = 20, l = 20, "pt"),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      # axis.text.y=element_text(margin = margin(1, unit = "cm"), vjust =1.5)
    )
}

#' Prepare dataset for plotting the proportion of conclusions
calculate_conclusion <- function(data, grouping_var, categorization_var) {
  data %>% 
    dplyr::select({{ grouping_var }}, {{ categorization_var }}) %>% 
    dplyr::count({{ grouping_var }}, {{ categorization_var }}) %>% 
    dplyr::group_by({{ grouping_var }}) %>%
    tidyr::complete({{ categorization_var }}, fill = list(n = 0)) %>% 
    dplyr::mutate(
      N = sum(n),
      relative_frequency = n / N,
      percentage = round(relative_frequency * 100),
    ) %>%
    ungroup()
}

#' Plot conclusions
plot_conclusion <- function(data, grouping_var, categorization_var, with_labels = FALSE, y_lab = NULL) {
  data <- 
    data |>  
    dplyr::mutate(label_text := paste0({{ grouping_var }}, "\n", "(N = " , N, ")"))
  
  plot <- 
    data |> 
    ggplot2::ggplot() +
    ggplot2::aes(
      y = label_text,
      x = relative_frequency,
      fill = {{ categorization_var }}
    ) +
    ggplot2::geom_bar(
      color = "black",
      stat = "identity",
      width = 0.8) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 100)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
    ggplot2::labs(
      fill = stringr::str_to_sentence(quo_name(enquo(categorization_var))),
      x = "Percentage",
      y = y_lab
    ) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      # axis.title.y = element_blank(),
      # legend.box = "horizontal",
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      # legend.position = "bottom",
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  if (with_labels) {
    plot <-
      plot +
      ggplot2::geom_text(
        data = dplyr::filter(data, percentage > 10),
        ggplot2::aes(label = scales::percent(round(percentage), scale = 1)),
        position = position_stack(vjust = 0.5),  # Adjust the vertical position of labels
        color = "white",  # Label text color
        size = 5  # Label text size
      )
  }
    
    return(plot)
}

#' for now input must be calculate_conclusion() result data
calculate_conclusion_robustness <- function(data, grouping_var, categorization_var) {
  if (missing(grouping_var)) {
    data %>%
      dplyr::group_by(paper_id) %>% 
      dplyr::summarise(
        robust = dplyr::if_else(all({{categorization_var}} == "Same conclusion"), "Robust", "Not Robust")
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::count(robust) %>%
      dplyr::mutate(
        N = sum(n),
        relative_frequency = n / N,
        percentage = round(relative_frequency * 100, 2)
      )
  } else {
    data %>% 
      dplyr::group_by(paper_id, {{grouping_var}}) %>%
      dplyr::summarise(
        robust = dplyr::if_else(all({{categorization_var}} == "Same conclusion"), "Robust", "Not Robust")
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::count({{grouping_var}}, robust) %>% 
      dplyr::group_by({{grouping_var}}) %>% 
      dplyr::mutate(
        N = sum(n),
        relative_frequency = n / N,
        percentage = round(relative_frequency * 100, 2)
      ) |> 
      dplyr::ungroup()
  }
}

plot_conclusion_robustness <- function(data, response_var, grouping_var = NULL, with_labels = TRUE) {
  plot <-
    data %>% 
    ggplot2::ggplot() +
    ggplot2::aes(
      x = {{response_var}},
      y = relative_frequency,
      fill = {{grouping_var}}
        ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.8) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1), labels = scales::percent_format(scale = 100)) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::labs(
      x = "Robustness of the Conclusions",
      y = "Percentage"
    ) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      # legend.box = "horizontal",
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      # legend.position = "bottom",
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  if (!rlang::quo_is_missing(rlang::enquo(grouping_var))) {
    legend_label <- stringr::str_to_sentence(stringr::str_replace_all(rlang::quo_name(rlang::enquo(grouping_var)), "_", " "))
    
    plot <-
      plot +
      ggplot2::labs(fill = legend_label) +
      ggplot2::theme(
        legend.position = "bottom"
      )
  }
  
  if (with_labels) {
    plot <-
      plot +
      ggplot2::geom_text(
        ggplot2::aes(x ={{response_var}}, y = relative_frequency, label = as.character(paste0(n, "/", N))),
        position = position_dodge(width = 0.8),
        vjust = -0.5,
        color = "black",  
        size = 5
      )
  }
  
  plot
}

#' Function to check different response to a question in case of multiple analysis
check_diff_response <- function(data, id_var, response_var) {
  data %>% 
    dplyr::count({{id_var}}, {{response_var}}) %>% 
    dplyr::group_by({{id_var}}) %>% 
    dplyr::mutate(n_per_analyst = n()) %>% 
    dplyr::arrange(desc(n_per_analyst)) %>%
    dplyr::filter(n_per_analyst > 1L)
}

#' Keep the first response
#' 
#' @description The function checks for multiple responses with a given id and flags and keeps only
#' the first response.
keep_first_response <- function(data, id_var, time_var) {
  data %>%
    dplyr::group_by({{id_var}}) %>% 
    dplyr::mutate(
      first_response = dplyr::if_else(
        {{time_var}} == min({{time_var}}),
        TRUE, FALSE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(first_response)
}

#' Function to calculate the count and percentage of the responses for a response variable
calculate_percentage <- function(data, response_var) {
  data %>% 
    dplyr::count({{response_var}}) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      N = sum(n),
      freq = round(n / N, 4),
      percentage = freq * 100
    ) 
}

#' Calculate standard correlation coefficient
standard_correlation_coefficient <- function(type_of_statistic, test_statistic, sample_size, df1, df2) {
  dplyr::if_else(type_of_statistic == "z",
    tanh(abs(test_statistic)*sqrt(1/(sample_size-3)))*sign(test_statistic),
    dplyr::if_else(type_of_statistic == "chi2",
            tanh(sqrt(abs(test_statistic))*sqrt(1/(sample_size-3)))*sign(test_statistic),
            dplyr::if_else(type_of_statistic == "t",
                    sqrt(abs(test_statistic)^2/(test_statistic^2+df1))*sign(test_statistic),
                    dplyr::if_else(type_of_statistic == "F",
                            sqrt(abs(sqrt(test_statistic))^2/(sqrt(test_statistic)^2+df2))*sign(test_statistic),
                            NA_real_)))
  )
}

#' Calculate Cohen's d
cohens_d <- function(r) {
  sign(r)*sqrt(4/((1/r^2)-1))
}

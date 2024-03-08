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
      {{ grouping_var }} := as.factor({{ grouping_var }}),
      is_within_region = dplyr::case_when(
        reanalysis_cohens_d >= tolarence_region_lower & reanalysis_cohens_d <= tolarence_region_upper ~ "Within tolerance region",
        reanalysis_cohens_d < tolarence_region_lower | reanalysis_cohens_d > tolarence_region_upper ~ "Outside of tolerance region",
        is.na(reanalysis_cohens_d) ~ "Missing"
      ),
      is_within_region = factor(is_within_region, levels = c("Within tolerance region", "Outside of tolerance region"))
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
plot_tolarence_region <- function(data, grouping_var, with_labels = FALSE, y_lab = NULL, x_lab = NULL) {
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
      color = "black",
      position = position_stack(reverse = TRUE)
      ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 1)) +
    ggplot2::scale_y_discrete(limits = rev) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
    viridis::scale_color_viridis(discrete = TRUE, direction = -1) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      # fill = "Within region"
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      # axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = element_blank(),
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
        aes(
          label = scales::percent(round(percentage), scale = 1),
          color = is_within_region
          ),
        position = position_stack(vjust = 0.5, reverse = TRUE), 
        size = 5  
      )
  }
  
  return(plot)
}

#' Prepare dataset for effect size robustness plot
calculate_estimate_range <- function(data, grouping_var) {
  data %>% 
    dplyr::select(paper_id, {{grouping_var}}, reanalysis_cohens_d) %>% 
    dplyr::group_by(paper_id, {{grouping_var}}) %>% 
    # dplyr::filter(reanalysis_cohens_d == min(reanalysis_cohens_d, na.rm = FALSE) | reanalysis_cohens_d == max(reanalysis_cohens_d, na.rm = FALSE)) %>% 
    dplyr::summarise(
      estimate_range = max(reanalysis_cohens_d, na.rm = TRUE) - min(reanalysis_cohens_d, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
}

#' Create effect size robustness plot
plot_rain <- function(data, grouping_var, response_var, x_lab, y_lab, trans = "log10", breaks = c(10, 100, 1000, 10000, 100000)) {
  data |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = {{grouping_var}}, y = {{response_var}}) +
    ggrain::geom_rain(rain.side = 'l') +
    ggplot2::labs(y = y_lab,
                  x = x_lab) +
    ggplot2::scale_y_continuous(
      trans = trans,
      breaks = breaks,
      labels = scales::comma
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_line()
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

#' plot percentage
plot_percentage <- function(data, grouping_var, categorization_var, with_labels = FALSE, x_lab = NULL, y_lab = NULL, legend_lab = NULL, with_sum = TRUE, reverse = TRUE, rev_limits = TRUE, coord_flip = FALSE) {
  
  if (with_sum) {
    new_labels <- setNames(
      as.character(data[[quo_name(enquo(grouping_var))]]),
      paste0(
        data[[quo_name(enquo(grouping_var))]],
        "\n",
        "(N = " ,
        data[["N"]],
        ")"
        )
      )
    
    data <-
      data |>
      dplyr::mutate(label := forcats::fct_recode({{ grouping_var }}, !!!new_labels))
  } else {
    data <-
      data |> 
      dplyr::rename(label := {{ grouping_var }})
  }
  
  plot <-
    data |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = label,
      y = relative_frequency,
      fill = {{ categorization_var }}
    ) +
    ggplot2::geom_bar(
      color = "black",
      stat = "identity",
      width = 0.8,
      position = position_stack(reverse = reverse)
      ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      labels = scales::percent_format(scale = 100)) +
    ggplot2::scale_x_discrete(
      expand = c(0, 0),
      limits = if (rev_limits) {
        rev(levels(droplevels(data$label))) 
      } else {
        levels(droplevels(data$label))
        }
      # labels = function(x) print(x)
      ) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
    viridis::scale_color_viridis(discrete = TRUE, direction = -1) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      fill = legend_lab
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      # axis.title.y = element_blank(),
      legend.box = "horizontal",
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      legend.position = "bottom",
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  if (with_labels) {
    plot <-
      plot +
      ggplot2::geom_text(
        data = dplyr::filter(data, percentage > 10),
        ggplot2::aes(label = scales::percent(round(percentage), scale = 1),
        color = {{ categorization_var }}),
        position = position_stack(vjust = 0.5, reverse = reverse),
        size = 5  # Label text size
      )
  }
    
  # Check if coord_flip is TRUE, then apply coord_flip()
  if (coord_flip) {
    plot <- plot + ggplot2::coord_flip()
  }
  
    return(plot)
}

# TODO: bad name, replace it later
plot_height <- function(data, grouping_var, categorization_var, with_labels = FALSE, x_lab = NULL, y_lab = NULL, legend_lab = NULL, with_sum = TRUE, reverse = TRUE, rev_limits = TRUE) {
  
  if (with_sum) {
    new_labels <- setNames(
      as.character(data[[quo_name(enquo(grouping_var))]]),
      paste0(
        data[[quo_name(enquo(grouping_var))]],
        "\n",
        "(N = " ,
        data[["N"]],
        ")"
      )
    )
    
    data <-
      data |>
      dplyr::mutate(label := forcats::fct_recode({{ grouping_var }}, !!!new_labels))
  } else {
    data <-
      data |> 
      dplyr::rename(label := {{ grouping_var }})
  }
  
  plot <-
    data |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = label,
      y = n,
      fill = {{ categorization_var }}
    ) +
    ggplot2::geom_bar(
      color = "black",
      size = 0.2,
      stat = "identity",
      width = 0.8,
      position = position_stack(reverse = reverse)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      # labels = scales::percent_format(scale = 100)
      ) +
    ggplot2::scale_x_discrete(
      # expand = c(0, 0),
      limits = if (rev_limits) {
        rev(levels(droplevels(data$label))) 
      } else {
        levels(droplevels(data$label))
      }
      # labels = function(x) print(x)
    ) +
    viridis::scale_fill_viridis(discrete = TRUE) + 
    viridis::scale_color_viridis(discrete = TRUE, direction = -1) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      fill = legend_lab
    ) +
    ggplot2::guides(color = "none") +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"),
      axis.ticks = ggplot2::element_blank(),
      # axis.title.y = element_blank(),
      legend.box = "horizontal",
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10, "pt"),
      legend.position = "bottom",
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  if (with_labels) {
    cutoff <- round(max(data[["N"]]) * 0.1)
    
    plot <-
      plot +
      ggplot2::geom_text(
        data = dplyr::filter(data, percentage > 10 & N > cutoff),
        ggplot2::aes(label = scales::percent(round(percentage), scale = 1),
                     color = {{ categorization_var }}),
        position = position_stack(vjust = 0.5, reverse = reverse),
        size = 4  # Label text size
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
        robust = dplyr::if_else(all({{categorization_var}} == "Same conclusion"), "Inferentially robust", "Inferentially not robust"),
        robust = factor(robust, levels = c("Inferentially robust", "Inferentially not robust"))
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::count(robust) %>%
      dplyr::mutate(
        N = sum(n),
        relative_frequency = n / N,
        percentage = round(relative_frequency * 100, 2)
      ) |> 
      dplyr::ungroup()
  } else {
    data %>% 
      dplyr::group_by(paper_id, {{grouping_var}}) %>%
      dplyr::summarise(
        robust = dplyr::if_else(all({{categorization_var}} == "Same conclusion"), "Inferentially robust", "Inferentially not robust"),
        robust = factor(robust, levels = c("Inferentially robust", "Inferentially not robust"))
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
      x = "Inferential robustness of studies",
      y = "Percentage of studies"
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
        legend.position = "bottom",
        legend.title = element_blank()
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
standard_correlation_coefficient <- function(type_of_statistic, test_statistic, sample_size, df1, df2, reanalysis_es_missing) {
  # Only calculate the correlation coefficient for cases where all the values needed are there
  if(reanalysis_es_missing == 1) {
    return(NA_real_)
  }
  
  result <- tryCatch(
    {
      case_result <- NA_real_  # Default result if none of the conditions match
      if (type_of_statistic == "z") {
        case_result <- tanh(abs(test_statistic)*sqrt(1/(sample_size-3)))*sign(test_statistic)
      } else if (type_of_statistic == "chi2") {
        case_result <- tanh(sqrt(abs(test_statistic))*sqrt(1/(sample_size-3)))*sign(test_statistic)
      } else if (type_of_statistic == "t") {
        case_result <- sqrt(abs(test_statistic)^2/(test_statistic^2+df1))*sign(test_statistic)
      } else if (type_of_statistic == "F") {
        case_result <- sqrt(abs(sqrt(test_statistic))^2/(sqrt(test_statistic)^2+df2))*sign(test_statistic)
      } else if (type_of_statistic == "r") {
        case_result <- test_statistic
      } else if (type_of_statistic == "tau") {
        case_result <- sin(.5 * test_statistic * pi)
      }
      case_result
    },
    warning = function(w) {
      message("Warning in standard_correlation_coefficient with input parameters:")
      message(paste("Type of statistic:", type_of_statistic))
      message(paste("Test statistic:", test_statistic))
      message(paste("Sample size:", sample_size))
      message(paste("df1:", df1))
      message(paste("df2:", df2))
      message("Warning message: ", w$message)
      NA_real_  # Return NA in case of warning
    },
    error = function(e) {
      message("Error in standard_correlation_coefficient with input parameters:")
      message(paste("Type of statistic:", type_of_statistic))
      message(paste("Test statistic:", test_statistic))
      message(paste("Sample size:", sample_size))
      message(paste("df1:", df1))
      message(paste("df2:", df2))
      message("Error message: ", e$message)
      NA_real_  # Return NA in case of error
    }
  )
  
  return(result)
}

#' Calculate Cohen's d
cohens_d <- function(r, type_of_statistic, test_statistic, sample_size) {
  dplyr::case_when(
    type_of_statistic %in% c("z", "chi2", "t", "F", "r", "tau") & !is.na(r) ~ round(sign(r)*sqrt(4/((1/r^2)-1)), 3),
    type_of_statistic == "reg_coeff" ~ test_statistic/sqrt(sample_size),
    TRUE ~ NA_real_
  )
}

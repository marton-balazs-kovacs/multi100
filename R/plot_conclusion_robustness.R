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

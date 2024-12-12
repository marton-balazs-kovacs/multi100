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




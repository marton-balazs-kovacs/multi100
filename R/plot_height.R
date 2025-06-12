plot_height <- function(data, grouping_var, categorization_var, with_labels = FALSE, x_lab = NULL, y_lab = NULL, legend_lab = NULL, with_sum = TRUE, reverse = TRUE, rev_limits = TRUE) {
  
  if (with_sum) {
    new_labels <- setNames(
      as.character(data[[quo_name(enquo(grouping_var))]]),
      paste0(
        data[[quo_name(enquo(grouping_var))]],
        "\n",
        "(𝑁 = " ,
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
      panel.grid = ggplot2::element_blank(),
      legend.key = element_rect(color = "black", linewidth = 0.2)
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
        size = 3  # Label text size
      )
  }
  
  return(plot)
}



#' Create effect size robustness plot
plot_rain <- function(data, grouping_var, response_var, x_lab, y_lab, trans = "log10", breaks = c(10, 100, 1000, 10000, 100000), color_by_group = FALSE) {
  p <- ggplot2::ggplot(data) +
    ggplot2::aes(x = {{ grouping_var }}, y = {{ response_var }})
  
  # Add fill aesthetic conditionally
  if (color_by_group) {
    p <- p + ggplot2::aes(fill = {{ grouping_var }})
  }
  
  # Add geom_rain with fixed color and alpha
  p <- p + ggrain::geom_rain(
    rain.side = 'r',
    color = "black",
    alpha = 0.6
  )
  
  # Add axis labels and y-axis transformation
  p <- p +
    ggplot2::labs(y = y_lab, x = x_lab) +
    ggplot2::scale_y_continuous(
      trans = trans,
      breaks = breaks,
      labels = scales::comma
    )
  
  # Add fill scale only if fill aesthetic is mapped
  if (color_by_group) {
    p <- p + viridis::scale_fill_viridis(discrete = TRUE)
  }
  
  # Apply theme
  p <- p + ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(),
    axis.ticks = ggplot2::element_blank(),
    legend.position = "none",
    axis.text = ggplot2::element_text(color = "black")
  )
  
  return(p)
}


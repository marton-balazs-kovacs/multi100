#' Create effect size robustness plot
plot_rain <- function(data, grouping_var, response_var, x_lab, y_lab, trans = "log10", breaks = c(10, 100, 1000, 10000, 100000)) {
  data |> 
    ggplot2::ggplot() +
    ggplot2::aes(x = {{grouping_var}}, y = {{response_var}}) +
    ggrain::geom_rain(rain.side = 'r') +
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


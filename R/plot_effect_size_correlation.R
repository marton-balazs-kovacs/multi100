plot_effect_size_correlation <- function(data, x_limits, y_limits, annotate_positions, fill_var = NULL, show_legend = FALSE) {
  # Based on: https://github.com/CenterForOpenScience/rpp
  colors <- c("Original" = "#440154FF", "Replication" = "#FDE725FF")
  
  # Fit the linear model
  lm_fit <- lm(effect_size ~ original_cohens_d, data = data)
  
  # Extract slope
  beta <- coef(lm_fit)[2]
  
  # Calculate the breaks based on the supplied limits
  x_breaks <- seq(from = x_limits[1], to = x_limits[2], by = 0.5) 
  y_breaks <- seq(from = y_limits[1], to = y_limits[2], by = 0.5) 
  
  # Creating an expression vector
  beta_label <- bquote(italic(beta) == .(round(beta, 2)))
  beta_label <- as.expression(beta_label)
  
  # Set font sizes based on whether legend is shown
  if (show_legend) {
    axis_title_size <- 22
    axis_text_size <- 20
    legend_text_size <- 14
    annotation_size <- 4
  } else {
    axis_title_size <- 35
    axis_text_size <- 30
    annotation_size <- 10
  }
  
  # Create the scatter plot base
  scatter <-
    data |> 
    ggplot(aes(x = original_cohens_d, y = effect_size)) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      color = "black",
      alpha = 0.2
    ) +
    annotate("text", x = annotate_positions[1], y = annotate_positions[2], label = beta_label, size = annotation_size, color = "black") +
    geom_rug(
      aes(x = original_cohens_d),
      size = 1,
      sides = "b",
      alpha = .6,
      color = I("#440154FF")  # Original
    ) +
    geom_rug(
      aes(y = effect_size),
      size = 1,
      sides = "l",
      alpha = .6,
      color = I("#FDE725FF")  # Replication
    ) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_abline(intercept = 0,
                slope = 1,
                color = "Grey60") +
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks
    ) +
    labs(
      x = expression("Original effect size in Cohen's " * italic("d")),
      y = expression("Re-analysis effect size in Cohen's " * italic("d"))
    )
  
  # Add points with or without fill mapping
  if (!is.null(fill_var)) {
    # Use .data pronoun for dynamic column access
    fill_mapping <- rlang::sym(fill_var)
    scatter <- scatter +
      geom_point(
        aes(fill = !!fill_mapping),
        shape = 21,
        color = "Grey30",
        alpha = 0.8
      ) +
      scale_fill_viridis_d(option = "D")
    
    # Add fill label if show_legend is TRUE
    if (show_legend) {
      scatter <- scatter +
        labs(fill = "Conclusion")
    }
  } else {
    scatter <- scatter +
      geom_point(
        color = "Grey30",
        size = 3.5,
        shape = 21,
        alpha = .8
      )
  }
  
  # Set theme based on whether legend is shown
  if (show_legend) {
    scatter <- scatter +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = legend_text_size),
        axis.title = element_text(size = axis_title_size, color = "black"),
        axis.text = element_text(size = axis_text_size, color = "black")
      )
  } else {
    scatter <- scatter +
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        axis.title = element_text(size = axis_title_size, color = "black"),
        axis.text = element_text(size = axis_text_size, color = "black")
      )
  }
 
  # Add density plots on the axes
  plot_with_margins <- ggExtra::ggMarginal(
    scatter,
    type = "density",
    margins = "both",
    xparams = list(fill = colors["Original"], alpha = 0.5),
    yparams = list(fill = colors["Replication"], alpha = 0.5)
  )
  
  return(plot_with_margins)
}

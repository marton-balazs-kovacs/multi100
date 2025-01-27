plot_effect_size_correlation <- function(data, x_limits, y_limits, annotate_positions) {
  # Based on: https://github.com/CenterForOpenScience/rpp
  colors <- c("Original" = "#440154FF", "Replication" = "#FDE725FF")
  
  # Fit the linear model
  lm_fit <- lm(effect_size ~ original_cohens_d, data = data)
  
  # Extract slope
  beta <- coef(lm_fit)[2]
  
  # Calculate the breaks based on the supplied limits
  x_breaks <- seq(from = x_limits[1], to = x_limits[2], by = 0.5) 
  
  y_breaks <- seq(from = y_limits[1], to = y_limits[2], by = 0.5) 
  
  # Create the scatter plot
  scatter <-
    data |> 
    ggplot(aes(x = original_cohens_d, y = effect_size, color = as.factor(colors))) +
    geom_point(color = "Grey30",
               shape = 21,
               alpha = .8) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      color = "black",
      alpha = 0.2
    ) +
    annotate("text", x = annotate_positions[1], y = annotate_positions[2], label = bquote(italic(beta) == .(round(beta, 2))), size = 4, color = "black") +
    geom_rug(
      aes(color = "Original"),
      size = 1,
      sides = "b",
      alpha = .6
    ) +
    geom_rug(
      aes(color = "Replication"),
      size = 1,
      sides = "l",
      alpha = .6
    ) +
    scale_color_manual(values = colors) + 
    geom_hline(aes(yintercept = 0), linetype = 2) +
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
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"),
      legend.position = "none",
      axis.title = element_text(size = 25),
      axis.text = element_text(size = 20)
    )
  
  # Add density plots on the axes
  ggExtra::ggMarginal(
    scatter,
    type = "density",
    margins = "both",
    xparams = list(fill = colors["Original"], alpha = 0.5),
    yparams = list(fill = colors["Replication"], alpha = 0.5)
  )
}

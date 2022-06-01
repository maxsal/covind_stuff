tvr_plot <- function(plot_data, state_name) {
  plot_data |>
    ggplot(aes(x = date, y = r_est)) +
    geom_hline(yintercept = 1, linetype = 2, color = "#FF9933") +
    geom_ribbon(aes(ymin = r_lower, ymax = r_upper), fill = "#138808", alpha = 0.5) +
    geom_line(size = 1, color = "#138808") +
    geom_point(shape = 3, size = 0.5) +
    scale_x_date(date_labels = "%b %e") +
    labs(
      title    = glue("Time-varying R estimate in {state_name}"),
      subtitle = glue("from {format(plot_data[, min(date)], '%B %e, %Y')} to {format(plot_data[, max(date)], '%B %e, %Y')}"),
      x        = "Date",
      y        =  "R(t)",
      caption  = glue("**Sources:** covid19india.org; covind19.org<br>",
                      "**\uA9 COV-IND-19 Study Group**")
    ) +
    theme_classic() +
    theme(
      text          = element_text(family = "Lato"),
      plot.title    = element_text(hjust = 0, face = "bold"),
      plot.subtitle = element_text(hjust = 0, color = "gray40"),
      plot.caption  = element_markdown(hjust = 0)
    )
}
bar_plot <- function(tmp, state_name) {
  tmp |>
    ggplot(aes(
      x = date,
      y = count,
      group = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")),
      fill = factor(Trend, levels = c("Recovered", "Fatalities", "New cases"))
    )) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = dbc) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%b %e") +
    guides(fill = guide_legend(title = "", override.aes = list(size = 1))) +
    labs(
      title    = glue("Daily number of COVID-19 cases, fatalities, and recovered in {state_name}"),
      subtitle = glue("from {format(tmp[, min(date)], '%B %e, %Y')} to {format(tmp[, max(date)], '%B %e, %Y')}"),
      x        = "Date",
      y        = "Count",
      caption  = "**Source:** covid19data.org<br>**\uA9 COV-IND-19 Study Group**"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      text            = element_text(family = "Lato"),
      legend.title    = element_text(size = 10),
      legend.text     = element_text(size = 8),
      legend.key.size = unit(0.3, "cm"),
      plot.title      = element_text(hjust = 0, face = "bold"),
      plot.subtitle   = element_text(hjust = 0, color = "gray40"),
      plot.caption    = element_markdown(hjust = 0)
    )
}
vax_bar_plot <- function(data, state_name) {
  
  data[place == state_name] |>
    ggplot(aes(x = date, y = daily_doses)) +
    geom_bar(stat = "identity", fill = "#138808") +
    scale_x_date(date_labels = "%B", date_breaks = "2 months") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = glue("Daily COVID-19 vaccine doses in {state_name}"),
      subtitle = glue("{format(data[place == state_name, min(date)], '%B %e, %Y')} to {format(data[place == state_name, max(date)], '%B %e, %Y')}"),
      x        = "Date",
      y        = "Daily vaccine doses",
      caption  = "\uA9 COV-IND-19 Study Group"
    ) +
    theme_classic() +
    theme(
      text               = element_text(family = "Lato"),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(hjust = 0, color = "gray40"),
      plot.caption       = element_text(hjust = 0, face = "bold"),
      panel.grid.major.y = element_line(color = "gray80", linetype = 2),
      axis.line.y        = element_blank(),
      axis.ticks.y       = element_blank()
    )
  
}

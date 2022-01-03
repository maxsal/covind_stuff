vax_dat <- merge.data.table(
  get_state_vax(),
  covid19india::pop[, !c("population")][, .SD[1], by = "place"],
  by = "place", all.x = TRUE
)[!is.na(total_doses)][date >= "2021-02-15"]

state_names <- vax_dat[, unique(place)]

vax_bar_plot <- function(name) {
  
  vax_dat[place == name] |>
    ggplot(aes(x = date, y = daily_doses)) +
    geom_bar(stat = "identity", fill = "#138808") +
    scale_x_date(date_labels = "%B", date_breaks = "2 months") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = glue("Daily COVID-19 vaccine doses in {name}"),
      subtitle = glue("{format(vax_dat[place == name, min(date)], '%B %e, %Y')} to {format(vax_dat[place == name, max(date)], '%B %e, %Y')}"),
      x        = "Date",
      y        = "Daily vaccine doses",
      caption  = "\uA9 COV-IND-19 Study Group"
    ) +
    theme_classic() +
      theme(
        text               = element_text(family = "Helvetica Neue"),
        plot.title         = element_text(face = "bold"),
        plot.subtitle      = element_text(hjust = 0, color = "gray40"),
        plot.caption       = element_text(hjust = 0, face = "bold"),
        panel.grid.major.y = element_line(color = "gray80", linetype = 2),
        axis.line.y        = element_blank(),
        axis.ticks.y       = element_blank()
      )
  
}

vax_bar_plot(name = "India")

max_date <- vax_dat[, max(date)]
if (!dir.exists(here("vax_plots", max_date))) {
  dir.create(here("vax_plots", max_date))
}

for (i in seq_along(state_names)) {
  
  tmp_plot <- vax_bar_plot(name = state_names[i])
  
  ggsave(filename = here("vax_plots", max_date, glue("{make_clean_names(state_names[i])}_vax_plot.pdf")),
                    plot = tmp_plot, width = 9, height = 5, device = cairo_pdf
  )
  
}

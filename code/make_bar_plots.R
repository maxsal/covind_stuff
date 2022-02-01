set_seed <- 46342
set.seed(set_seed)

max_date   <- Sys.Date() - 1

d <- covid19india::get_all_data()[date <= max_date]
f <- unique(d[, place])

dbc <- daily_barplot_colors[!names(daily_barplot_colors) %in% "Cases"]

options(warn = -1)
for (i in seq_along(f)) {
  
  if (grepl("\\*", f[i])) { next }
  
  cli::cli_alert(glue("{f[i]} [{i}/{length(f)} ({round(i*100/length(f))}%)]..."))
  
  tmp <- d[place == f[i] & date <= max_date & daily_cases > 0 & daily_recovered > 0 & daily_deaths > 0][, .(date, daily_cases, daily_deaths, daily_recovered)]
  
  tmp <- data.table::melt(tmp, id.vars = "date", variable.name = "Trend", value.name = "count")[, Trend := data.table::fcase(
    Trend == "daily_cases", "New cases",
    Trend == "daily_deaths", "Fatalities",
    Trend == "daily_recovered", "Recovered"
  )]
  
  inc_plt <- tmp |>
    ggplot(aes(x = date, y = count, group = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")), fill = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = dbc) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%b %e") +
    guides(fill = guide_legend(title = "", override.aes = list(size = 1))) +
    labs(
      title    = glue("Daily number of COVID-19 cases, fatalities, and recovered in {f[i]}"),
      subtitle = glue("from {format(tmp[, min(date)], '%B %e, %Y')} to {format(tmp[, max(date)], '%B %e, %Y')}"),
      x        = "Date",
      y        = "Count",
      caption  = "**Source:** covid19data.org<br>**\uA9 COV-IND-19 Study Group**"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      text            = element_text(family = "Helvetica Neue"),
      legend.title    = element_text(size = 10),
      legend.text     = element_text(size = 8),
      legend.key.size = unit(0.3, "cm"),
      plot.title      = element_text(hjust = 0, face = "bold"),
      plot.subtitle   = element_text(hjust = 0, color = "gray40"),
      plot.caption    = element_markdown(hjust = 0)
    )
  
  if (!dir.exists(glue("bar_plots/{max_date}/"))) {
    dir.create(glue("bar_plots/{max_date}/"))
  }

  cairo_pdf(filename = glue("bar_plots/{max_date}/{max_date}_{f[i]}_bar_plot.pdf"),
      width = 12, height = 6)
    print(inc_plt)
  dev.off()
  
}

options(warn = 1)
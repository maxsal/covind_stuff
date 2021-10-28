set_seed <- 46342
set.seed(set_seed)

start_date <- "2020-03-24"
max_date   <- Sys.Date() - 1

d <- covid19india::get_all_data()[date <= max_date]
f <- unique(d[, place])

dbc <- daily_barplot_colors[!names(daily_barplot_colors) %in% "Cases"]

options(warn = -1)
for (i in seq_along(f)) {
  
  cli::cli_alert(glue("{f[i]} [{i}/{length(f)} ({round(i*100/length(f))}%)]..."))
  
  tmp <- d[place == f[i] & date >= start_date & date <= max_date & daily_cases > 0 & daily_recovered > 0 & daily_deaths > 0][, .(date, daily_cases, daily_deaths, daily_recovered)]
  
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
      subtitle = glue("as of {format(max(d$date), '%B %e, %Y')}"),
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
  
  tvr_plt <- d[date >= "2020-03-24" & place == f[i]] |>
    ggplot(aes(x = date, y = r_est)) +
    geom_hline(yintercept = 1, linetype = 2, color = "#FF9933") +
    geom_ribbon(aes(ymin = r_lower, ymax = r_upper), fill = "#138808", alpha = 0.5) +
    geom_line(size = 1, color = "#138808") +
    geom_point(shape = 3, size = 0.5) +
    scale_x_date(date_labels = "%b %e") +
    labs(
      title    = glue("Time-varying R estimate in {f[i]}"),
      subtitle = glue("from {format(as.Date(start_date), '%B %e, %Y')} to {format(max(d$date), '%B %e, %Y')}"),
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
  
  if (!dir.exists(here("stack_plots", glue("{max_date}")))) {
    dir.create(path = here("stack_plots", glue("{max_date}")), recursive = T)
  }
  
  png(filename = here("stack_plots", glue("{max_date}"), glue("{f[i]}_plot.png")),
      width = 10, height = 8, units = "in", res = 320)
    print(inc_plt / tvr_plt)
  dev.off()
  
}

options(warn = 1)
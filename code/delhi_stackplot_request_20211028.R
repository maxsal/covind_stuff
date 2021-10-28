ally::libri(data.table, covid19india, tidyverse, glue, ggtext, patchwork)

state <- "Delhi"

taco <- get_all_data(mohfw = FALSE)[place == state]

start_date <- as.Date("2020-03-24")
max_date   <- as.Date("2021-08-05")

tmp <- taco[date >= start_date & date <= max_date & daily_cases > 0 & daily_recovered > 0 & daily_deaths > 0][, .(date, daily_cases, daily_deaths, daily_recovered)]

tmp <- data.table::melt(tmp, id.vars = "date", variable.name = "Trend", value.name = "count")[, Trend := data.table::fcase(
  Trend == "daily_cases", "New cases",
  Trend == "daily_deaths", "Fatalities",
  Trend == "daily_recovered", "Recovered"
)][]

daily_barplot_colors <- c(
  "Fatalities" = "#ED553B",
  "New cases"  = "#f2c82e",
  "Recovered"  = "#138808"
)

inc_plt <- tmp |>
  ggplot(aes(x = date, y = count, group = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")), fill = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")))) +
  
  geom_vline(xintercept = as.Date("2021-01-27"), linetype = 2, color = "gray40") +
  annotate(geom = "text", x = as.Date("2021-01-25"), y = tmp[, .(value = sum(count)), by = date][, max(value)] * 1.05,
           label = "January 27", size = 3, color = "gray40", hjust = 1, family = "Lato") +
  geom_vline(xintercept = as.Date("2021-04-19"), linetype = 2, color = "gray40") +
  annotate(geom = "text", x = as.Date("2021-04-21"), y = tmp[, .(value = sum(count)), by = date][, max(value)] * 1.05,
           label = "April 19", size = 3, color = "gray40", hjust = 0, family = "Lato") +
  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = daily_barplot_colors) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  guides(fill = guide_legend(title = "", override.aes = list(size = 1))) +
  labs(
    title    = glue("Daily number of COVID-19 cases, fatalities, and recovered in {state}"),
    subtitle = glue("{format(start_date, '%B %e, %Y')} to {format(max_date, '%B %e, %Y')}"),
    x        = "Date",
    y        = "Count"
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

tvr_plt <- taco[between(date, start_date, max_date)] |>
  ggplot(aes(x = date, y = r_est)) +
  
  geom_vline(xintercept = as.Date("2021-01-27"), linetype = 2, color = "gray40") +
  annotate(geom = "text", x = as.Date("2021-01-25"), y = taco[, max(r_est, na.rm = TRUE)] * 1.05,
           label = "January 27", size = 3, color = "gray40", hjust = 1, family = "Lato") +
  geom_vline(xintercept = as.Date("2021-04-19"), linetype = 2, color = "gray40") +
  annotate(geom = "text", x = as.Date("2021-04-21"), y = taco[, max(r_est, na.rm = TRUE)] * 1.05,
           label = "April 19", size = 3, color = "gray40", hjust = 0, family = "Lato") +
  
  geom_hline(yintercept = 1, linetype = 2, color = "#FF9933") +
  geom_ribbon(aes(ymin = r_lower, ymax = r_upper), fill = "#138808", alpha = 0.5) +
  geom_line(size = 1, color = "#138808") +
  geom_point(shape = 3, size = 0.5) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(
    title    = glue("Time-varying R estimate in {state}"),
    subtitle = glue("{format(start_date, '%B %e, %Y')} to {format(max_date, '%B %e, %Y')}"),
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

cairo_pdf(filename = glue("~/Downloads/{state}_plot.pdf"),
    width = 10, height = 8)
  print(inc_plt / tvr_plt)
dev.off()

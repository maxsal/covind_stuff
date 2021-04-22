library(here)
source("libraries.R")
source("functions.R")

start_date <- "2020-03-15"
max_date   <- "2021-03-15"

tmp <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                col_types = cols()) %>%
  clean_names()

tmp_plt <- tmp %>%
  dplyr::filter(date_ymd >= start_date & date_ymd <= max_date) %>%
  dplyr::select(-c(total_confirmed, total_deceased, total_recovered, date)) %>%
  tidyr::pivot_longer(names_to = "Trend", values_to = "count", -c(date_ymd)) %>%
  dplyr::mutate(
    Trend = as.factor(case_when(
      Trend == "daily_confirmed" ~ "New cases",
      Trend == "daily_deceased" ~ "Fatalities",
      Trend == "daily_recovered" ~ "Recovered"
    ))
  ) %>%
  ggplot(aes(x = date_ymd, y = count, group = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")), fill = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")))) +
  geom_vline(xintercept = as.Date("2020-12-31"), color = "gray40", linetype = 2) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = daily_barplot_colors) +
  scale_y_continuous(labels = scales::comma) +
  guides(fill = guide_legend(title = "", override.aes = list(size = 1))) +
  labs(
    title    = glue("Daily number of COVID-19 cases, fatalities, and recovered in India"),
    subtitle = glue("as of {format(as.Date(max_date), '%B %e')}"),
    x        = "Date",
    y        = "Count",
    caption  = "**\uA9 COV-IND-19 Study Group**<br>**Source:** covid19data.org"
  ) +
  covind_theme() +
  theme(legend.position = "top",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        plot.margin = margin(10, 20, 10, 10))

tmp_plt

ggsave(plot     = tmp_plt,
       filename = here("fig", glue("india_barplot_{max_date}.pdf")),
       width    = 12, height = 6,
       device   = cairo_pdf)



# time-vaying R plot -----------
d <- data.table::fread("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/2021-03-31/everything.csv")

plt <- d[place == "India" & date >= start_date & date <= max_date, .(place, date, r_est, r_lower, r_upper)] %>%
  drop_na() %>%
  ggplot(aes(x = date, y = r_est)) +
  geom_vline(xintercept = as.Date("2020-12-31"), color = "gray40", linetype = 2) +
  annotate("segment", y = 1, yend = 1, x = as.Date(start_date), xend = as.Date(max_date), color = "#FF9933", size = 0.5) +
  geom_ribbon(aes(ymin = r_lower, ymax = r_upper), fill = "#138808", alpha = 0.5) +
  geom_line(color = "#138808", size = 1) +
  geom_point(shape = 3, size = 1) +
  labs(
    title = "Time-varying R estimate for India",
    subtitle = glue("as of {format(as.Date(max_date), '%e %B')}"),
    x = "Date",
    y = "R(t)",
    caption = "**\uA9 COV-IND-19 Study Group**"
  ) +
  covind_theme() +
  theme(plot.margin = margin(10, 20, 10, 10))
plt

ggsave(plot     = plt,
       filename = here("fig", glue("india_tvrplot_{max_date}.pdf")),
       width    = 12, height = 6,
       device   = cairo_pdf)








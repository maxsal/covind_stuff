d <- data.table::fread("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/2021-03-31/everything.csv")

start_date <- "2020-03-15"
max_date   <- "2021-03-15"

plt <- d[place == "India" & date >= start_date & date <= max_date, .(place, date, r_est, r_lower, r_upper)] %>%
  drop_na() %>%
  ggplot(aes(x = date, y = r_est)) +
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

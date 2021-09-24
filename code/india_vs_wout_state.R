pacman::p_load(covid19india, data.table, ggplot2, patchwork, janitor,
               ggtext, here, glue, lubridate)


fclean_names <- function(x) {
  setnames(x, names(x), janitor::make_clean_names(names(x)))
}

obs <- covid19india::get_all_data()

comp_state <- "Kerala"

obs <- obs |>
  fclean_names() |>
  DT(date >= "2021-02-15" & place %in% c("India", comp_state)) |>
  DT(, .(place, date, daily_cases, daily_deaths))

obs_india <- obs[place == "India"]
obs_state <- obs[place == comp_state]

obs_state$daily_cases  <- obs_india$daily_cases - obs_state$daily_cases
obs_state$daily_deaths <- obs_india$daily_deaths - obs_state$daily_deaths

obs <- rbindlist(list(obs_india, obs_state))[, `:=` (
  date  = as.Date(date),
  place = fifelse(place == comp_state, as.character(glue("India without {comp_state}")), place)
  )] |>
  melt(id.vars = c("place", "date"), variable.name = "cases", value.name = "count") |>
  DT(, count := fifelse(count < 0, 0, count))


case_plt <- obs[cases == "daily_cases" & place == "India"] |>
  ggplot(aes(x = date, y = count, fill = place)) +
  geom_col() +
  geom_col(data = obs[cases == "daily_cases" & place == glue("India without {comp_state}")]) +
  # theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B", breaks = sort(unique(lubridate::floor_date(unique(obs$date), "month")))) +
  labs(
    title = "Cases",
    x     = "Date",
    y     = "Daily reported case count" 
  )

death_plt <- obs[cases == "daily_deaths" & place == "India"] |>
  ggplot(aes(x = date, y = count, fill = place)) +
  geom_col() +
  geom_col(data = obs[cases == "daily_deaths" & place == glue("India without {comp_state}")]) +
  # theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B", breaks = sort(unique(lubridate::floor_date(unique(obs$date), "month")))) +
  labs(
    title = "Deaths",
    x     = "Date",
    y     = "Daily reported death count" 
  )

plt <- case_plt + death_plt

plt <- plt +
  plot_annotation(
    title    = glue("COVID-19 cases and deaths in India with and without {comp_state}"),
    subtitle = glue("{format(min(obs$date), '%B %e, %Y')} to {format(max(obs$date), '%B %e, %Y')}"),
    tag_levels = c("A")
  ) &
  theme(
    text              = element_text(family = "Lato"),
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 14, hjust = 0, color = "gray40"),
    plot.caption      = element_markdown(size = 12, hjust = 0),
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Lato", face = "bold")
  )

ggsave(filename = here("fig", glue("case_death_{comp_state}.pdf")),
       plot     = plt,
       height   = 6,
       width    = 15,
       units = "in", device = cairo_pdf)

rm(obs, obs_india, obs_state)

obs <- covid19india::get_all_data()

obs <- obs |>
  fclean_names() |>
  DT(date >= "2021-01-01" & place %in% c("India", comp_state)) |>
  DT(, .(place, date, daily_cases, daily_deaths))

obs_india <- obs[place == "India"]
obs_state <- obs[place == comp_state]

obs_state$daily_cases  <- obs_india$daily_cases - obs_state$daily_cases
obs_state$daily_deaths <- obs_india$daily_deaths - obs_state$daily_deaths

obs <- rbindlist(list(obs_india, obs_state))[, `:=` (
  date  = as.Date(date),
  place = fifelse(place == comp_state, as.character(glue("India without {comp_state}")), place),
  cfr   = daily_deaths / daily_cases
)]

plt2 <- obs |>
  ggplot(aes(x = date, y = cfr, color = place)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%B", breaks = sort(unique(lubridate::floor_date(unique(obs$date), "month")))) +
  labs(
    title    = glue("COVID-19 daily CFR in India with and without {comp_state}"),
    subtitle = glue("{format(min(obs$date), '%B %e, %Y')} to {format(max(obs$date), '%B %e, %Y')}"),
    x     = "Date",
    y     = "Case-fatality rate"
  ) +
  theme(plot.subtitle = element_text(color = "gray40"))


ggsave(filename = here("fig", glue("cfr_{comp_state}.pdf")),
       plot     = plt2,
       height   = 6,
       width    = 10,
       units = "in", device = cairo_pdf)


library(covid19india)
library(data.table)
library(ggplot2)
library(patchwork)
library(janitor)
library(ggtext)
library(here)
library(glue)

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
  melt(id.vars = c("place", "date"), variable.name = "cases", value.name = "count")

case_plt <- obs[cases == "daily_cases" & place == "India"] |>
  ggplot(aes(x = date, y = count, fill = place)) +
  geom_col() +
  geom_col(data = obs[cases == "daily_cases" & place == glue("India without {comp_state}")]) +
  # theme_classic() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B") + 
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
  scale_x_date(date_labels = "%B") + 
  labs(
    title = "Deaths",
    x     = "Date",
    y     = "Daily reported death count" 
  )

plt <- case_plt + death_plt

ggsave(filename = here("fig", glue("case_death_{comp_state}.pdf")),
       plot     = plt,
       height   = 6,
       width    = 10,
       units = "in", device = cairo_pdf)

rm(obs, obs_india, obs_state)

obs <- covid19india::get_all_data()

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
  place = fifelse(place == comp_state, as.character(glue("India without {comp_state}")), place),
  cfr   = daily_deaths / daily_cases
)]

plt2 <- obs |>
  ggplot(aes(x = date, y = cfr, color = place)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%B") + 
  labs(
    title = "Case-fatality rate",
    x     = "Date",
    y     = "Case-fatality rate"
  )


ggsave(filename = here("fig", glue("cfr_{comp_state}.pdf")),
       plot     = plt2,
       height   = 6,
       width    = 10,
       units = "in", device = cairo_pdf)


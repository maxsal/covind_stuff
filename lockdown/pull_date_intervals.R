pacman::p_load(tidyverse, glue, here)

scenarios <- c("2021-03-01", "2021-03-15", "2021-03-30",
               "2021-04-15", "2021-04-30", "no_intervention")

for (i in seq_along(scenarios)) {
  
  tmp_filename <- glue("{scenarios[i]}_mh_smooth1_data.txt")
  
  if (i == 1) {
    p <- read_tsv(here("lockdown", "rupam", "output",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("lockdown", "rupam", "output",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

dates <- as.Date(c("2021-03-15", "2021-03-30", "2021-04-15", "2021-04-30", "2021-05-15"))

p |>
  mutate(
    value    = round(value / 1e6, 2),
    lower_ci = round(lower_ci / 1e6, 2),
    upper_ci = round(upper_ci / 1e6, 2)
  ) |>
  filter(date %in% dates) |>
  as.data.frame()

pacman::p_load(tidyverse, janitor, glue, ggtext, here, patchwork, httr)
source(here("code", "functions", "functions.R"))

count_dat <- get_count_data()
test_dat  <- get_testing_data()

cases_dat <- count_dat %>%
  filter(abbrev %in% c("India", "mh", "dl"),
         date >= "2021-02-01") %>%
  select(date, place, daily_cases) %>%
  pivot_wider(
    names_from = "place",
    values_from = "daily_cases",
    id_cols = "date"
  ) %>%
  mutate(
    `India (w/o MH + DL)_cases` = India - Delhi - Maharashtra
  ) %>%
  rename(
    India_cases       = India,
    Delhi_cases       = Delhi,
    Maharashtra_cases = Maharashtra
  )

dtests_dat <- test_dat %>%
  filter(place %in% c("India", "Maharashtra", "Delhi"),
         date >= "2021-02-01") %>%
  select(date, place, daily_tests) %>%
  pivot_wider(
    names_from = "place",
    values_from = "daily_tests",
    id_cols = "date"
  ) %>%
  mutate(
    `India (w/o MH + DL)_tests` = India - Delhi - Maharashtra
  ) %>%
  rename(
    India_tests       = India,
    Delhi_tests       = Delhi,
    Maharashtra_tests = Maharashtra
  )

merged <- cases_dat %>%
  left_join(dtests_dat, by = "date") %>%
  mutate(
    India_tpr                 = India_cases / India_tests,
    Delhi_tpr                 = Delhi_cases / Delhi_tests,
    Maharashtra_tpr           = Maharashtra_cases / Maharashtra_tests,
    `India (w/o MH + DL)_tpr` = `India (w/o MH + DL)_cases` /
      `India (w/o MH + DL)_tests`
  )

max_date <- merged %>% pull(date) %>% max()

merged %>% write_xlsx(here("data", "output", glue("table_{max_date}.xlsx")))

merged %>%
  pivot_longer(
    names_to = "stat",
    values_to = "value",
    -date
  ) %>%
  filter(str_detect(stat, "cases")) %>%
  ggplot(aes(x = date, y = value, group = stat, color = stat)) +
  geom_line() +
  labs(title = "Daily cases") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggsave(filename = here("data", "output", "cases_plot.pdf"), width = 7, height = 5,
         device = cairo_pdf)

merged %>%
  pivot_longer(
    names_to = "stat",
    values_to = "value",
    -date
  ) %>%
  filter(str_detect(stat, "tests")) %>%
  ggplot(aes(x = date, y = value, group = stat, color = stat)) +
  geom_line() +
  labs(title = "Daily tests") +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggsave(filename = here("data", "output", "tests_plot.pdf"), width = 7, height = 5,
         device = cairo_pdf)

merged %>%
  pivot_longer(
    names_to = "stat",
    values_to = "value",
    -date
  ) %>%
  filter(str_detect(stat, "tpr")) %>%
  ggplot(aes(x = date, y = value, group = stat, color = stat)) +
  geom_line() +
  labs(title = "TPR") +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggsave(filename = here("data", "output", "tpr_plot.pdf"), width = 7, height = 5,
         device = cairo_pdf)

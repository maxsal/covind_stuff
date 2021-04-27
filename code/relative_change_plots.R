library(tidyverse)
library(janitor)
library(glue)
library(ggtext)
library(here)

today <- Sys.Date() - 2
n_lag <- 7

d <- read_csv(glue::glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{today}/everything.csv"),
              col_types = cols()) %>%
  filter(place == "India") %>%
  dplyr::select(date, cases, daily_cases, total_tests, daily_tests) %>%
  dplyr::mutate(
    daily_tpr = daily_cases / daily_tests
  ) %>%
  dplyr::mutate(
    tests_change = (daily_tests - dplyr::lag(daily_tests, n_lag)) / dplyr::lag(daily_tests, n_lag),
    tpr_change   = (daily_tpr   - dplyr::lag(daily_tpr, n_lag)) / dplyr::lag(daily_tpr, n_lag),
    cases_change = (daily_cases - dplyr::lag(daily_cases, n_lag)) / dplyr::lag(daily_cases, n_lag)
  )

coeff <- d %>% pull(daily_tpr) %>% max(., na.rm = T) / d %>% pull(daily_tests) %>% max(., na.rm = T)

d %>%
  filter(date >= (today - 30)) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_tests), stat = "identity", size = 0.1, fill = "#FF9933", alpha = 0.5) +
  geom_smooth(aes(y = daily_tpr * (1/coeff)), formula = "y ~ x", color = "#138808", method = "loess", span = 0.1, se = FALSE) +
  scale_y_continuous(name = "Number of daily tests", labels = scales::comma,
                     sec.axis = sec_axis(~. * coeff, name = "Test positive rate (%)")) +
  labs(
    title = "COVID-19: <b style='color:#FF9933'>Daily tests</b> and <b style='color:#138808'>test positive rate</b> in India",
    subtitle = glue("data through {format(today, '%B %e, %Y')}"),
    x     = "Date",
    caption = "**Source:** covid19india.org<br>**\uA9 COV-IND-19 Study Group**"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_markdown(size = 18, face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 14),
    plot.caption = element_markdown(hjust = 0)
  )
ggsave(here("fig", "test_v_tpr_abs_comp.pdf"), width = 9, height = 5, device = cairo_pdf)
ggsave(here("fig", "test_v_tpr_abs_comp.png"), width = 9, height = 5, units = "in", dpi = 320)

med_case_change <- d %>% filter(date >= (today - 30)) %>% pull(cases_change) %>% median()
med_test_change <- d %>% filter(date >= (today - 30)) %>% pull(tests_change) %>% median()

d %>%
  filter(date >= (today - 30)) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0, color = "gray40", linetype = 2, size = 1) +
  geom_hline(yintercept = med_test_change, color = "#FF9933", linetype = 2, size = 1) +
  geom_hline(yintercept = med_case_change, color = "#138808", linetype = 2, size = 1) +
  
  annotate(geom = "text", label = glue("Median {n_lag}-day change: {round(med_test_change*100, 2)}%"),
           color = "#FF9933", x = today, y = med_test_change - 0.03, hjust = 1, size = 2) +
  
  annotate(geom = "text", label = glue("Median {n_lag}-day change: {round(med_case_change*100, 2)}%"),
           color = "#138808", x = today, y = med_case_change + 0.03, hjust = 1, size = 2) +
  
  geom_smooth(aes(y = tests_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#FF9933") +
  # geom_smooth(aes(y = tpr_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#138808") +
  geom_smooth(aes(y = cases_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#138808") +
  # scale_y_continuous(name = "Number of daily tests", labels = scales::comma,
  #                    sec.axis = sec_axis(~. * coeff, name = "Test positive rate (%)")) +
  labs(
    title   = "COVID-19: Relative change in <b style='color:#138808'>daily cases</b> and <b style='color:#FF9933'>daily tests</b> in India",
    subtitle = glue("data through {format(today, '%B %e, %Y')}"),
    x       = "Date",
    y       = glue("{n_lag}-day percent change"),
    caption = "**Source:** covid19india.org<br>**\uA9 COV-IND-19 Study Group**"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_markdown(size = 18, face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 14),
    plot.caption = element_markdown(hjust = 0)
  )
ggsave(here("fig", "test_v_tpr_change_comp.pdf"), width = 9, height = 5, device = cairo_pdf)
ggsave(here("fig", "test_v_tpr_change_comp.png"), width = 9, height = 5, units = "in", dpi = 320)

d %>%
  filter(date >= (today - 30)) %>%
  ggplot(aes(x = date, y = tpr_change / tests_change)) +
  geom_smooth(method = "loess", formula = "y ~ x", se = FALSE, span = 0.2)

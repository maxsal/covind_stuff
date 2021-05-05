library(tidyverse)
library(janitor)
library(glue)
library(ggtext)
library(here)

today <- Sys.Date() - 2
n_lag <- 7

d <- read_csv(glue::glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{today}/everything.csv"),
              col_types = cols()) %>%
  dplyr::filter(place == "India") %>%
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

india <- d %>%
  filter(date >= (today - 30)) %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0, color = "gray40") +
  geom_hline(yintercept = med_test_change, color = "#FF9933", linetype = 2, size = 1) +
  geom_hline(yintercept = med_case_change, color = "#138808", linetype = 2, size = 1) +
  
  geom_smooth(aes(y = tests_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#FF9933") +
  # geom_smooth(aes(y = tpr_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#138808") +
  geom_smooth(aes(y = cases_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#138808") +
  # scale_y_continuous(name = "Number of daily tests", labels = scales::comma,
  #                    sec.axis = sec_axis(~. * coeff, name = "Test positive rate (%)")) +
  
  annotate(geom = "label", label = glue("Median {n_lag}-day change: {round(med_test_change*100, 2)}%"),
           color = "#FF9933", x = today, y = med_test_change - 0.03, hjust = 1, size = 2.5,
           alpha = 0.75) +
  
  annotate(geom = "label", label = glue("Median {n_lag}-day change: {round(med_case_change*100, 2)}%"),
           color = "#138808", x = today, y = med_case_change + 0.03, hjust = 1, size = 2.5,
           alpha = 0.75) +
  
  labs(
    title   = "COVID-19: Relative change in <b style='color:#138808'>daily cases</b> and <b style='color:#FF9933'>daily tests</b> in India",
    subtitle = glue("{format(today - 30, '%B %e, %Y')} to {format(today, '%B %e, %Y')}"),
    x       = "Date",
    y       = glue("{n_lag}-day percent change"),
    caption = "**Source:** covid19india.org<br>**\uA9 COV-IND-19 Study Group**"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_markdown(size = 18, face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 14),
    plot.caption = element_markdown(hjust = 0)
  )
india
ggsave(here("fig", "test_v_case_change_comp.pdf"), width = 9, height = 5, device = cairo_pdf)
ggsave(here("fig", "test_v_case_change_comp.png"), width = 9, height = 5, units = "in", dpi = 320)

x <- read_csv("https://covidtracking.com/data/download/national-history.csv",
              col_types = cols()) %>%
  clean_names() %>%
  select(date, tests = total_test_results, daily_tests = total_test_results_increase)

y <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
              col_types = cols()) %>%
  filter(`Country/Region` == "US") %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  pivot_longer(
    names_to = "date",
    values_to = "count",
    -`Country/Region`
  ) %>%
  mutate(date = as.Date(date, format = "%m/%e/%y")) %>%
  rename(country = `Country/Region`) %>%
  arrange(date) %>%
  mutate(
    daily_cases = count - dplyr::lag(count)
  )

us_end <- as.Date("2021-02-15")

m <- y %>% left_join(x, by = "date") %>%
  dplyr::mutate(
    tests_change = (daily_tests - dplyr::lag(daily_tests, n_lag)) / dplyr::lag(daily_tests, n_lag),
    cases_change = (daily_cases - dplyr::lag(daily_cases, n_lag)) / dplyr::lag(daily_cases, n_lag),
    daily_tpr = daily_cases / daily_tests
  ) %>%
  filter(date >= "2020-11-01" & date <= us_end)

us_med_case_change <- m %>% pull(cases_change) %>% median()
us_med_test_change <- m %>% pull(tests_change) %>% median()


us <- m %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept = 0, color = "gray40") +
  geom_hline(yintercept = us_med_test_change, color = "#3C3B6E", linetype = 2, size = 1) +
  geom_hline(yintercept = us_med_case_change, color = "#B22234", linetype = 2, size = 1) +
  
  geom_smooth(aes(y = tests_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#3C3B6E") +
  # geom_smooth(aes(y = tpr_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#138808") +
  geom_smooth(aes(y = cases_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#B22234") +
  # scale_y_continuous(name = "Number of daily tests", labels = scales::comma,
  #                    sec.axis = sec_axis(~. * coeff, name = "Test positive rate (%)")) +
  annotate(geom = "label", label = glue("Median {n_lag}-day change: {round(us_med_test_change*100, 2)}%"),
           color = "#3C3B6E", x = us_end, y = us_med_test_change - 0.03, hjust = 1, size = 2.5,
           alpha =  0.75) +
  
  annotate(geom = "label", label = glue("Median {n_lag}-day change: {round(us_med_case_change*100, 2)}%"),
           color = "#B22234", x = us_end, y = us_med_case_change + 0.03, hjust = 1, size = 2.5,
           alpha = 0.75) +
  
  labs(
    title   = "COVID-19: Relative change in <b style='color:#B22234'>daily cases</b> and <b style='color:#3C3B6E'>daily tests</b> in the US",
    subtitle = glue("{format(as.Date('2020-11-01'), '%B %e, %Y')} to {format(us_end, '%B %e, %Y')}"),
    x       = "Date",
    y       = glue("{n_lag}-day percent change"),
    caption = "**Source:** JHU CSSE GitHub (cases); The COVID Tracking Projects (tests)<br>**\uA9 COV-IND-19 Study Group**"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_markdown(size = 18, face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 14),
    plot.caption = element_markdown(hjust = 0)
  )
us
ggsave(here("fig", "us_test_v_case_change_comp.pdf"), width = 9, height = 5, device = cairo_pdf)
ggsave(here("fig", "us_test_v_case_change_comp.png"), width = 9, height = 5, units = "in", dpi = 320)

gA <- ggplotGrob(india)
gB <- ggplotGrob(us)

png(filename = here("fig", glue("us_v_india_comp_plot.png")),
    width = 10, height = 12, units = "in", res = 320)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
dev.off()

cairo_pdf(filename = here("fig", glue("us_v_india_comp_plot.pdf")),
    width = 10, height = 12)
grid::grid.newpage()
grid::grid.draw(rbind(gA, gB))
dev.off()

coeff_d <- .25 / d %>% filter(date >= (today - 30)) %>% pull(daily_tests) %>% max(., na.rm = T)
tpr_d_mag <- d %>% filter(date == "2021-04-20") %>% pull(daily_tpr)

india2 <- d %>%
  filter(date >= (today - 30)) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_tests), stat = "identity", fill = "#FF9933", alpha = 0.5) +
  geom_bar(aes(y = daily_cases), stat = "identity", fill = "#138808") +
  geom_line(aes(y = daily_tpr / coeff_d), size = 1) +
  annotate("segment", x = as.Date("2021-04-20"), xend = as.Date("2021-04-20"), y = tpr_d_mag / coeff_d, yend = 0.125 / coeff_d) +
  annotate("label", label = "Test-positive rate",
           x = as.Date("2021-04-20"), y = 0.13 / coeff_d, alpha = 0.8) +
  labs(
    title   = "COVID-19: <b style='color:#138808'>Daily cases</b> and <b style='color:#FF9933'>daily tests</b> in India",
    subtitle = glue("{format(today - 30, '%B %e, %Y')} to {format(today, '%B %e, %Y')}"),
    x       = "Date",
    y       = glue("Daily count"),
    caption = "**Source:** covid19india.org<br>**\uA9 COV-IND-19 Study Group**"
  ) +
  scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~.*coeff_d, name = "Test-positive rate", labels = scales::percent)) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_markdown(size = 18, face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 14),
    plot.caption = element_markdown(hjust = 0)
  )
india2

coeff_m <- .25 / m %>% pull(daily_tests) %>% max(., na.rm = T)
tpr_m_mag <- m %>% filter(date == "2021-01-30") %>% pull(daily_tpr)

us2 <- m %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = daily_tests), stat = "identity", fill = "#3C3B6E", alpha = 0.5) +
  geom_bar(aes(y = daily_cases), stat = "identity", fill = "#B22234") +
  geom_line(aes(y = daily_tpr / coeff_m), size = 1) +
  annotate("segment", x = as.Date("2021-01-30"), xend = as.Date("2021-01-30"), y = tpr_m_mag / coeff_m, yend = 0.125 / coeff_m) +
  annotate("label", label = "Test-positive rate",
           x = as.Date("2021-01-30"), y = 0.13 / coeff_m, alpha = 0.8) +
  labs(
    title   = "COVID-19: <b style='color:#B22234'>Daily cases</b> and <b style='color:#3C3B6E'>daily tests</b> in the US",
    subtitle = glue("{format(as.Date('2020-11-01'), '%B %e, %Y')} to {format(us_end, '%B %e, %Y')}"),
    x       = "Date",
    y       = glue("Daily count"),
    caption = "**Source:** JHU CSSE GitHub (cases); The COVID Tracking Projects (tests)<br>**\uA9 COV-IND-19 Study Group**"
  ) +
  scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~.*coeff_m, name = "Test-positive rate", labels = scales::percent)) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_markdown(size = 18, face = "bold"),
    plot.subtitle = element_text(color = "gray40", size = 14),
    plot.caption = element_markdown(hjust = 0)
  )
us2

gC <- ggplotGrob(india2)
gD <- ggplotGrob(us2)

png(filename = here("fig", glue("us_v_india_bar_plot.png")),
    width = 10, height = 12, units = "in", res = 320)
grid::grid.newpage()
grid::grid.draw(rbind(gC, gD))
dev.off()

cairo_pdf(filename = here("fig", glue("us_v_india_bar_plot.pdf")),
          width = 10, height = 12)
grid::grid.newpage()
grid::grid.draw(rbind(gC, gD))
dev.off()

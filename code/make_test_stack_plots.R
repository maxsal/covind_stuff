library(tidyverse)
library(janitor)
library(glue)
library(ggtext)
library(here)

today <- Sys.Date() - 1
n_lag <- 7

dat <- read_csv(glue::glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{today}/everything.csv"),
              col_types = cols()) %>%
  dplyr::mutate(
    daily_tpr = daily_cases / daily_tests
  ) %>%
  group_by(place) %>%
  arrange(date) %>%
  dplyr::mutate(
    tests_change = (daily_tests - dplyr::lag(daily_tests, n_lag)) / dplyr::lag(daily_tests, n_lag),
    tpr_change   = (daily_tpr   - dplyr::lag(daily_tpr, n_lag)) / dplyr::lag(daily_tpr, n_lag),
    cases_change = (daily_cases - dplyr::lag(daily_cases, n_lag)) / dplyr::lag(daily_cases, n_lag)
  ) %>%
  ungroup()

abbrevs <- unique(dat$abbrev)

for (i in seq_along(abbrevs)) {
  
  d <- dat %>%
    dplyr::filter(abbrev == abbrevs[i])
  
  tmp_place <- d %>% pull(place) %>% unique()

  coeff_d <- .25 / d %>% dplyr::filter(date >= (today - 30)) %>% pull(daily_tests) %>% max(., na.rm = T)
  tpr_d_mag <- d %>% dplyr::filter(date == "2021-04-20") %>% pull(daily_tpr)

  bar_plt <- d %>%
    dplyr::filter(date >= (today - 30)) %>%
    ggplot(aes(x = date)) +
    geom_bar(aes(y = daily_tests), stat = "identity", fill = "#FF9933", alpha = 0.5) +
    geom_bar(aes(y = daily_cases), stat = "identity", fill = "#138808") +
    geom_line(aes(y = daily_tpr / coeff_d), size = 1) +
    annotate("segment", x = as.Date("2021-04-20"), xend = as.Date("2021-04-20"), y = tpr_d_mag / coeff_d, yend = 0.125 / coeff_d) +
    annotate("label", label = "Test-positive rate",
             x = as.Date("2021-04-20"), y = 0.13 / coeff_d, alpha = 0.8) +
    labs(
      title   = glue("COVID-19: <b style='color:#138808'>Daily cases</b> and <b style='color:#FF9933'>daily tests</b> in {tmp_place}"),
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
  bar_plt

  med_case_change <- d %>% dplyr::filter(date >= (today - 30)) %>% pull(cases_change) %>% median()
  med_test_change <- d %>% dplyr::filter(date >= (today - 30)) %>% pull(tests_change) %>% median()
  
  
  line_plt <- d %>%
    dplyr::filter(date >= (today - 30)) %>%
    ggplot(aes(x = date)) +
    geom_hline(yintercept = 0, color = "gray40") +
    geom_hline(yintercept = med_test_change, color = "#FF9933", linetype = 2, size = 1) +
    geom_hline(yintercept = med_case_change, color = "#138808", linetype = 2, size = 1) +
    
    geom_smooth(aes(y = tests_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#FF9933") +
    geom_smooth(aes(y = cases_change), method = "loess", formula = "y ~ x", span = 0.3, se = FALSE, color = "#138808") +
    
    annotate(geom = "label", label = glue("Median {n_lag}-day change: {round(med_test_change*100, 2)}%"),
             color = "#FF9933", x = today, y = med_test_change - 0.03, hjust = 1, size = 2.5,
             alpha = 0.75) +
    
    annotate(geom = "label", label = glue("Median {n_lag}-day change: {round(med_case_change*100, 2)}%"),
             color = "#138808", x = today, y = med_case_change + 0.03, hjust = 1, size = 2.5,
             alpha = 0.75) +
    
    labs(
      title   = glue("COVID-19: Relative change in <b style='color:#138808'>daily cases</b> and <b style='color:#FF9933'>daily tests</b> in {tmp_place}"),
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
  line_plt
  
  gA <- ggplotGrob(bar_plt)
  gB <- ggplotGrob(line_plt)
  
  if (!dir.exists(here("test_stack_plots", glue("{today}")))) {
    dir.create(path = here("test_stack_plots", glue("{today}")), recursive = T)
  }
  
  png(filename = here("test_stack_plots", glue("{today}"), glue("{tmp_place}_test_stack_plot.png")),
      width = 10, height = 12, units = "in", res = 320)
  grid::grid.newpage()
  grid::grid.draw(rbind(gA, gB))
  dev.off()
  
  
}

system("git status")
system("git add .")
system("git commit -m 'push new test stack plots'")
system("git push")

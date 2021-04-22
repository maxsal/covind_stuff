library(tidyverse)
library(janitor)
library(glue)
library(ggtext)

wave_2_start <- as.Date("2021-02-15")
today <- Sys.Date() - 1

d <- read_csv("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/2021-04-20/everything.csv",
              col_types = cols()) %>%
  dplyr::filter(place == "India") %>%
  mutate(
    daily_tpr = daily_cases / daily_tests
  )

w1 <- d %>%
  dplyr::filter(date < wave_2_start)
w2 <- d %>%
  dplyr::filter(date >= wave_2_start)

w1a <- w1 %>%
  dplyr::filter(cases >= 500)
w1b <- w1 %>%
  dplyr::filter(daily_cases >= w2 %>% filter(date == wave_2_start) %>% pull(daily_cases))


tmp <- w1a

wave_tab_stats <- function(dat, n_lag = 30) {
  
  list(
    "max_daily_cases" = tibble(
      "count" = dat %>% dplyr::filter(daily_cases == max(daily_cases, na.rm = T)) %>% pull(daily_cases),
      "date"  = dat %>% dplyr::filter(daily_cases == max(daily_cases, na.rm = T)) %>% pull(date)
    ),
    "max_eff_r" = tibble(
      "r_est" = dat %>% dplyr::filter(r_est == max(r_est, na.rm = T)) %>% pull(r_est),
      "date" = dat %>% dplyr::filter(r_est == max(r_est, na.rm = T)) %>% pull(date),
      "daily_cases" = dat %>% dplyr::filter(r_est == max(r_est, na.rm = T)) %>% pull(daily_cases),
      "total_cases" = dat %>% dplyr::filter(r_est == max(r_est, na.rm = T)) %>% pull(cases)
    ),
    "max_daily_deaths" = tibble(
      "count" = dat %>%
        dplyr::filter(daily_deaths < 2000) %>%
        dplyr::filter(daily_deaths == max(daily_deaths, na.rm = T)) %>%
        pull(daily_deaths),
      "date" = dat %>%
        dplyr::filter(daily_deaths < 2000) %>%
        dplyr::filter(daily_deaths == max(daily_deaths, na.rm = T)) %>%
        pull(date)
    ),
    "max_daily_tpr" = tibble(
      "daily_tpr" = dat %>%
        dplyr::filter(daily_tpr == max(daily_tpr, na.rm = T)) %>%
        pull(daily_tpr),
      "date" = dat %>%
        dplyr::filter(daily_tpr == max(daily_tpr, na.rm = T)) %>%
        pull(date),
      "daily_cases" = dat %>%
        dplyr::filter(daily_tpr == max(daily_tpr, na.rm = T)) %>%
        pull(daily_cases),
      "test" = dat %>%
        dplyr::filter(daily_tpr == max(daily_tpr, na.rm = T)) %>%
        pull(daily_tests)
    ),
    "max_daily_tests" = tibble(
      "daily_tests" = dat %>%
        dplyr::filter(daily_tests == max(daily_tests, na.rm = T)) %>%
        pull(daily_tests),
      "date" = dat %>%
        dplyr::filter(daily_tests == max(daily_tests, na.rm = T)) %>%
        pull(date)
    ),
    "quick_stats" = tibble(
      "total_cases"  = dat %>% pull(daily_cases) %>% sum(., na.rm = T),
      "total_deaths" = dat %>% pull(daily_deaths) %>% sum(., na.rm = T),
      "cfr" = dat %>% pull(daily_deaths) %>% sum(., na.rm = T) / dat %>% pull(daily_cases) %>% sum(., na.rm = T)
    ),
    "rel_case_inc" = dat %>%
      dplyr::select(date, daily_cases) %>%
      mutate(
        lag_date = dplyr::lag(date, n_lag),
        lag_cases = dplyr::lag(daily_cases, n_lag)
      ) %>%
      mutate(
        change = (daily_cases - lag_cases) / lag_cases
      ) %>%
      filter(
        change == max(change, na.rm = T)
      ),
    "rel_death_inc" = dat %>%
      dplyr::select(date, daily_deaths) %>%
      mutate(
        lag_date = dplyr::lag(date, n_lag),
        lag_deaths = dplyr::lag(daily_deaths, n_lag)
      ) %>%
      mutate(
        change = (daily_deaths - lag_deaths) / lag_deaths
      ) %>%
      filter(
        change == max(change, na.rm = T)
      ),
    "rel_tpr_inc" = dat %>%
      dplyr::select(date, daily_tpr) %>%
      mutate(
        lag_date = dplyr::lag(date, n_lag),
        lag_tpr = dplyr::lag(daily_tpr, n_lag)
      ) %>%
      mutate(
        change = (daily_tpr - lag_tpr) / lag_tpr
      ) %>%
      filter(
        change == max(change, na.rm = T)
      )
  )
  
}

a <- w1a %>% wave_tab_stats()
b <- w1b %>% wave_tab_stats()
c <- w2  %>% wave_tab_stats()

d %>%
  dplyr::filter(date >= "2020-03-24") %>%
  ggplot(aes(x = date, y = r_est)) +
  geom_hline(yintercept = 1, linetype = 2, color = "#FF9933") +
  geom_ribbon(aes(ymin = r_lower, ymax = r_upper), fill = "#138808", alpha = 0.5) +
  geom_line(size = 1, color = "#138808") +
  geom_point(shape = 3, size = 0.5) +
  labs(
    title    = "Time-varying R estimate in India",
    subtitle = glue("from March 24, 2020 to {format(max(d$date), '%B %e, %Y')}"),
    x        = "Date",
    y        =  "R(t)",
    caption  = glue("**Sources:** covid19india.org; covind19.org<br>",
                    "**\uA9 COVI-IND-19 Study Group**")
  ) +
  theme_classic() +
  theme(
    text          = element_text(family = "Lato"),
    plot.title    = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, color = "gray40"),
    plot.caption  = element_markdown(hjust = 0)
  )
ggsave(here("fig", glue("r_est_india_plot_{Sys.Date()}.pdf")), width = 7, height = 5, device = cairo_pdf)

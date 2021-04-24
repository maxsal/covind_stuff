library(tidyverse)
library(janitor)
library(glue)
library(ggtext)
library(gt)

wave_2_start <- as.Date("2021-02-15")
today <- Sys.Date() - 1
n_lag <- 30

d <- read_csv(glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{today}/everything.csv"),
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
        # dplyr::filter(daily_deaths < 2000) %>%
        dplyr::filter(daily_deaths == max(daily_deaths, na.rm = T)) %>%
        pull(daily_deaths),
      "date" = dat %>%
        # dplyr::filter(daily_deaths < 2000) %>%
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

a <- w1a %>% wave_tab_stats(n_lag = n_lag)
b <- w1b %>% wave_tab_stats(n_lag = n_lag)
c <- w2  %>% wave_tab_stats(n_lag = n_lag)

col_1 <- c("Maximimum # Daily New Cases",
           "Maximum Effective R", "Maximum # Daily reported deaths",
           "Maximum daily TPR", "Maximum # Daily tests done",
           "Total # cases during this period", "Total # deaths during this period",
           "Case fatality rates",
           glue("Biggest {n_lag}-day relative increase in cases"),
           glue("Biggest {n_lag}-day relative increase in deaths"),
           glue("Biggest {n_lag}-day relative increase in TPR"))

col_2 <- c(
  glue("{format(a$max_daily_cases$count, big.mark = ',')} ({format(a$max_daily_cases$date, '%B %e')})"),
  glue("{round(a$max_eff_r$r_est, 2)} ({format(a$max_eff_r$date, '%B %e')}, daily cases = {format(a$max_eff_r$daily_cases, big.mark = ',')}, total cases = {format(a$max_eff_r$total_cases, big.mark = ',')})"),
  glue("{format(a$max_daily_deaths$count, big.mark = ',')} ({format(a$max_daily_deaths$date, '%B %e')})"),
  glue("{round(a$max_daily_tpr$daily_tpr * 100, 1)}% ({format(a$max_daily_tpr$date, '%B %e')}: {format(a$max_daily_tpr$daily_cases, big.mark = ',')}, {format(a$max_daily_tpr$test, big.mark = ',')})"),
  glue("{format(a$max_daily_tests$daily_tests, big.mark = ',')} ({format(a$max_daily_tests$date, '%B %e')})"),
  glue("{format(a$quick_stats$total_cases, big.mark= ',')}"),
  glue("{format(a$quick_stats$total_deaths, big.mark = ',')}"),
  glue("{round(a$quick_stats$cfr * 100, 1)}%"),
  glue("{format(round(a$rel_case_inc$change + 1, 2), nsmall = 2)}x, {format(round(a$rel_case_inc$change * 100, 2), nsmall = 2)}% ({format(a$rel_case_inc$lag_cases, big.mark = ',')} on {format(a$rel_case_inc$lag_date, '%B %e')} to {format(a$rel_case_inc$daily_cases, big.mark = ',')} on {format(a$rel_case_inc$date, '%B %e')})"),
  glue("{format(round(a$rel_death_inc$change + 1, 2), nsmall = 2)}x, {format(round(a$rel_death_inc$change * 100, 2), nsmall = 2)}% ({format(a$rel_death_inc$lag_deaths, big.mark = ',')} on {format(a$rel_death_inc$lag_date, '%B %e')} to {format(a$rel_death_inc$daily_deaths, big.mark = ',')} on {format(a$rel_death_inc$date, '%B %e')})"),
  glue("{format(round(a$rel_tpr_inc$change + 1, 2), nsmall = 2)}x, {format(round(a$rel_tpr_inc$change * 100, 2), nsmall = 2)}% ({round(a$rel_tpr_inc$lag_tpr*100, 2)}% on {format(a$rel_tpr_inc$lag_date, '%B %e')} to {round(a$rel_tpr_inc$daily_tpr * 100, 2)}% on {format(a$rel_tpr_inc$date, '%B %e')})")
)

get_col <- function(a) {
  
  c(
    glue("{format(a$max_daily_cases$count, big.mark = ',')} ({format(a$max_daily_cases$date, '%B %e')})"),
    glue("{round(a$max_eff_r$r_est, 2)} ({format(a$max_eff_r$date, '%B %e')}, daily = {format(a$max_eff_r$daily_cases, big.mark = ',')}, total = {format(a$max_eff_r$total_cases, big.mark = ',')})"),
    glue("{format(a$max_daily_deaths$count, big.mark = ',')} ({format(a$max_daily_deaths$date, '%B %e')})"),
    glue("{round(a$max_daily_tpr$daily_tpr * 100, 1)}% ({format(a$max_daily_tpr$date, '%B %e')}: {format(a$max_daily_tpr$daily_cases, big.mark = ',')} cases, {format(a$max_daily_tpr$test, big.mark = ',')} tests)"),
    glue("{format(a$max_daily_tests$daily_tests, big.mark = ',')} ({format(a$max_daily_tests$date, '%B %e')})"),
    glue("{format(a$quick_stats$total_cases, big.mark= ',')}"),
    glue("{format(a$quick_stats$total_deaths, big.mark = ',')}"),
    glue("{round(a$quick_stats$cfr * 100, 1)}%"),
    glue("{format(round(a$rel_case_inc$change + 1, 2), nsmall = 2)}x, {format(round(a$rel_case_inc$change * 100, 2), nsmall = 2)}% ({format(a$rel_case_inc$lag_cases, big.mark = ',')} on {format(a$rel_case_inc$lag_date, '%B %e')} to {format(a$rel_case_inc$daily_cases, big.mark = ',')} on {format(a$rel_case_inc$date, '%B %e')})"),
    glue("{format(round(a$rel_death_inc$change + 1, 2), nsmall = 2)}x, {format(round(a$rel_death_inc$change * 100, 2), nsmall = 2)}% ({format(a$rel_death_inc$lag_deaths, big.mark = ',')} on {format(a$rel_death_inc$lag_date, '%B %e')} to {format(a$rel_death_inc$daily_deaths, big.mark = ',')} on {format(a$rel_death_inc$date, '%B %e')})"),
    glue("{format(round(a$rel_tpr_inc$change + 1, 2), nsmall = 2)}x, {format(round(a$rel_tpr_inc$change * 100, 2), nsmall = 2)}% ({round(a$rel_tpr_inc$lag_tpr*100, 2)}% on {format(a$rel_tpr_inc$lag_date, '%B %e')} to {round(a$rel_tpr_inc$daily_tpr * 100, 2)}% on {format(a$rel_tpr_inc$date, '%B %e')})")
  )
  
}

col_2 <- get_col(a)
col_3 <- get_col(b)
col_4 <- get_col(c)

col_2[3] <- "2,004 (June 16, reporting blip); 1,281 (September 15)"
col_3[3] <- "2,004 (June 16, reporting blip); 1,281 (September 15)"

tib <- tibble(
  "Stats" = col_1,
  "March 24, 2020 - February 14, 2021" = col_2,
  "June 3, 2020 - February 14, 2021" = col_3,
  "tmp" = col_4
)

names(tib)[names(tib) == "tmp"] <- glue("February 15, 2021 - {format(today - 1, '%B %e, %Y')}")

tib %>%
  gt() %>%
  # format table body text
  tab_style(
    style     = cell_text(size = px(10), font = "helvetica"),
    locations = cells_body()
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(vars(Stats))
  ) %>%
  # format column names
  tab_style(
    style = cell_text(
      size      = px(10),
      color     = "#5e5e5e",
      font      = "helvetica",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) %>%
  # random formatting
  tab_options(
    column_labels.border.top.style    = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.bottom.color    = "#0000001A",
    data_row.padding                  = px(4),
    # table.border.bottom.color = "#334422",
    source_notes.font.size = 8,
  ) %>%
  # column widths
  cols_width(
    vars(Stats) ~ px(210),
    vars(`March 24, 2020 - February 14, 2021`) ~ px(275),
    vars(`June 3, 2020 - February 14, 2021`) ~ px(275),
    # vars(R, CFR) ~ px(75),
    everything() ~ px(300)
  ) %>%
  cols_align(
    align   = "center",
    columns = everything()
  ) %>%
  # title
  tab_header(
    title    = md("**Comparing COVID-19 Waves in India**"),
    subtitle = glue("data through {format(today - 1, '%B %e')}")
  ) %>%
  # caption
  tab_source_note(
    source_note = md(glue(
      "**\uA9 COV-IND-19 Study Group**<br>**Source data:** covid19india.org; covind19.org"
    ))
  ) %>%
  # add and format column spanners
  tab_spanner(
    label   = "Wave 1",
    # columns = vars(`June 3, 2020 - February 14, 2021`)
    columns = vars(`March 24, 2020 - February 14, 2021`, `June 3, 2020 - February 14, 2021`)
  ) %>%
  tab_spanner(
    label   = "Wave 2",
    columns = 4
  ) %>% 
  cols_move_to_start(vars(Stats)) %>%
  tab_style(
    style = cell_text(
      size      = px(10),
      color     = "#5e5e5e",
      font      = "helvetica",
      transform = "uppercase"
    ),
    locations = cells_column_spanners(spanners = c("Wave 1", "Wave 2"))
  ) %>%
  # adjust title font
  tab_style(
    style     = list(cell_text(font = "helvetica", size = px(16))),
    locations = list(cells_title(groups = "title"))
  ) %>%
  # adjust subtitle font
  tab_style(
    style     = list(cell_text(font = "helvetica", size = px(13))),
    locations = list(cells_title(groups = "subtitle"))
  ) %>%
  cols_align(
    align = c("left"),
    columns = vars(Stats)
  )

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

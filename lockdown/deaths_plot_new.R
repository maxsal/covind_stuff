library(tidyverse)
library(plotly)
library(lubridate)
library(ggsci)
library(ggrepel)
library(janitor)
library(glue)
library(here)
library(ggtext)

mh <- TRUE
kl <- TRUE

obs <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                col_types = cols()) %>%
  clean_names() %>%
  rename(
    daily_cases  = daily_confirmed,
    daily_deaths = daily_deceased,
    total_cases  = total_confirmed,
    total_deaths = total_deceased
  ) %>%
  select(-date) %>%
  rename(date = date_ymd) %>%
  filter(date >= "2021-02-15") %>% 
  mutate(cfr = daily_deaths/daily_cases)

scenarios <- c("2021-03-01", "2021-03-15", "2021-03-30",
               "2021-04-15", "2021-04-30", "no_intervention")

for (i in seq_along(scenarios)) {
  
  if (mh == TRUE) {
    tmp_filename <- glue("{scenarios[i]}_mh_smooth1_data.txt")
  } else {
    tmp_filename <- glue("{scenarios[i]}_smooth1_data.txt")
  }
  
  if (i == 1) {
    p <- read_tsv(here("lockdown", "data", "final",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("lockdown", "data", "final",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

p <- p %>%
  mutate(
    scenario = paste(
      trimws(format(as.Date(scenario), '%B')),
      trimws(format(as.Date(scenario), '%e'))
    )
  ) %>%
  mutate(
    scenario = case_when(
      scenario == "NA NA" ~ "No intervention",
      T ~ scenario
    )
  ) %>%
  drop_na(incidence)

# mar <- read_tsv("march25_smooth_forecast.txt")
d <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
              col_types = cols()) %>%
  clean_names() %>%
  select(-date) %>%
  select(date = date_ymd, daily_cases = daily_confirmed, daily_deaths = daily_deceased,
         total_cases = total_confirmed, total_deaths = total_deceased, 
         everything()) %>%
  filter(date >= "2021-02-15")

if (kl == TRUE) {
  taco <- read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                   col_types = cols()) %>%
    clean_names() %>%
    select(date = date_ymd, status, value = kl) %>%
    pivot_wider(
      names_from  = "status",
      values_from = "value",
      id_cols     = "date"
    ) %>%
    select(date, daily_cases = Confirmed, daily_deaths = Deceased) %>%
    mutate(cfr = daily_deaths / daily_cases) %>%
    mutate(cfr_t7 = zoo::rollmean(cfr, k = 7, fill = NA, align = "right"))
  d <- d %>% left_join(taco %>% select(date, cfr, cfr_t7), by = "date")
} else if (mh == TRUE) {
  taco <- read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                   col_types = cols()) %>%
    clean_names() %>%
    select(date = date_ymd, status, value = mh) %>%
    pivot_wider(
      names_from  = "status",
      values_from = "value",
      id_cols     = "date"
    ) %>%
    select(date, daily_cases = Confirmed, daily_deaths = Deceased) %>%
    mutate(cfr = daily_deaths / daily_cases) %>%
    mutate(cfr_t7 = zoo::rollmean(cfr, k = 7, fill = NA, align = "right"))
  d <- d %>% left_join(taco %>% select(date, cfr, cfr_t7), by = "date")
} else {
  d <- d  %>%
    mutate(cfr = daily_deaths / daily_cases) %>%
    mutate(cfr_t7 = zoo::rollmean(x = cfr, k = 7, fill = NA, align = "right"))
}

# cfr <- read_csv(here("lockdown", "data", "revised", "cfr.csv"),
#                 col_types = cols()) %>%
#   select(place, date, period, cfr = smooth_cfr)

if (mh == TRUE & kl == TRUE) {
  # cfr <- cfr %>% filter(place == "Maharashtra")
  tmp_title    <- "Predicted number of daily COVID-19 deaths using Maharashtra lockdown schedule"
  tmp_subtitle <- "Using Kerala CFR schedule; February 15, 2021 to May 15, 2021"
  tmp_repel_y  <- c(0, 4000, 4000, 4000, 4000)
  tmp_nudge    <- 500
  tmp_filename <- "deaths_kl_mh.pdf"
} else if (mh == TRUE & kl == FALSE) {
  tmp_title    <- "Predicted number of daily COVID-19 deaths using Maharashtra lockdown schedule"
  tmp_subtitle <- "Using Maharashtra CFR schedule; February 15, 2021 to May 15, 2021"
  tmp_repel_y  <- c(400, 9000, 9000, 9000, 9000)
  tmp_nudge    <- 1000
  tmp_filename <- "deaths_mh.pdf"
} else if (kl == TRUE) {
  tmp_title    <- "Predicted number of daily COVID-19 deaths using India lockdown schedule"
  tmp_subtitle <- "Using Kerala CFR schedule; February 15, 2021 to May 15, 2021"
  tmp_repel_y  <- c(250, 4000, 4000, 4000, 4000)
  tmp_nudge    <- 500
  tmp_filename <- "deaths_kl.pdf"
} else {
  # cfr <- cfr %>% filter(place == "India")
  tmp_title    <- "Predicted number of daily COVID-19 deaths using India lockdown schedule"
  tmp_subtitle <- "Using India CFR schedule; February 15, 2021 to May 15, 2021"
  tmp_repel_y  <- c(250, 4000, 4000, 4000, 4000)
  tmp_nudge    <- 500
  tmp_filename <- "deaths.pdf"
}

# cfr <- read_tsv("march25_cfr.txt")



# [1] "March 1"         "March 15"        "March 30"        "April 15"        "April 25"       
# [6] "No intervention"

##march 1
mar.01 <- obs %>% 
  filter(date <= "2021-03-01") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(p %>%  
            filter(scenario == "March 1") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(d %>% select(date, cfr = cfr_t7)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "March 1") 

#march 15
mar.15 <- obs %>% 
  filter(date <= "2021-03-15") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(p %>%  
            filter(scenario == "March 15") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(d %>% select(date, cfr = cfr_t7)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "March 15") %>% 
  drop_na()


##march 30
mar.30 <- obs %>% 
  filter(date <= "2021-03-30") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(p %>%  
            filter(scenario == "March 30") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(d %>% select(date, cfr = cfr_t7)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "March 30")%>% 
  drop_na()



##april 15
apr.15 <- obs %>% 
  filter(date <= "2021-04-15") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(p %>%  
            filter(scenario == "April 15") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(d %>% select(date, cfr = cfr_t7)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "April 15")%>% 
  drop_na()

##april 30
apr.30 <- obs %>% 
  filter(date <= "2021-04-30") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(p %>%  
            filter(scenario == "April 30") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(d %>% select(date, cfr = cfr_t7)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "April 30")%>% 
  drop_na()


total <- obs %>% 
  filter(date <= "2021-05-31") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_column(scenario = "Observed") %>% 
  add_row(mar.01) %>% 
  add_row(mar.15) %>% 
  add_row(mar.30) %>% 
  add_row(apr.15) %>%
  add_row(apr.30) 

# ggplotly(total %>% 
#            filter(scenario != "March 1") %>%
#            nest(data = c(date, incidence)) %>% 
#            mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
#                   fitted = purrr::map(m, `[[`, "fitted")) %>% 
#            select(-m) %>% 
#            unnest(cols = c(data, fitted)) %>% 
#            ggplot(aes(x = date, y = fitted)) + 
#            #geom_point() + 
#            geom_line(aes(colour = scenario)))

total.smoothed <- total %>% 
  filter(scenario != "March 1") %>% 
  filter(date <= "2021-05-31") %>% 
  nest(data = c(date, incidence)) %>% 
  mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
         fitted = purrr::map(m, `[[`, "fitted")) %>% 
  select(-m) %>% 
  unnest(cols = c(data, fitted))


total.smoothed.plot <- total.smoothed %>% 
  filter(scenario == "Observed") %>% 
  filter(date <= "2021-05-15")  %>% 
  # add_row(total.smoothed %>% 
  #           filter(scenario == "No intervention") %>% 
  #           filter(date >= "2021-05-02")) %>%
  add_row(total.smoothed %>% 
            filter(scenario == "March 15", 
                   date >= "2021-03-09")) %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "March 30", 
                   date >= "2021-03-24")) %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "April 15", 
                   date >= "2021-04-08")) %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "April 30", 
                   date >= "2021-04-21")) %>% 
  mutate(scenario = factor(scenario, levels = c("Observed", "March 15", "March 30", 
                                                "April 15", "April 30")))%>%
  filter(date <= "2021-05-15") 

deaths.p <- total.smoothed.plot %>% 
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_colour_lancet() + 
  xlab("Date") + 
  ylab("Daily deaths") + 
  geom_vline(data = total.smoothed.plot %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  geom_label_repel(data = total.smoothed.plot %>% 
                     group_by(scenario) %>% 
                     filter(fitted == max(fitted)) %>% 
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     filter(!(scenario %in% c("Observed", "No intervention"))), 
                   aes(x = date, 
                       y = fitted, 
                       label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " deaths"),
                       color = scenario,
                       family = "Lato"), 
                   nudge_y = tmp_nudge, 
                   nudge_x = -10, 
                   size = 4,
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text_repel(data = total.smoothed.plot %>% 
                    group_by(scenario) %>% 
                    filter(date == min(date)) %>% 
                    dplyr::ungroup() %>% 
                    select(scenario, date, fitted) %>% 
                    mutate(text = c("Observed data", "Intervention  1", "Intervention  2", 
                                    "Intervention  3", "Intervention  4"), 
                           x = as.Date(c("2021-03-01", "2021-03-09", "2021-03-24", "2021-04-08", "2021-04-21")), 
                           y = tmp_repel_y), 
                  aes(x = x, 
                      y = y, 
                      label = text,
                      color = scenario,
                      family = "Lato"), 
                  nudge_x      = -5,
                  size         = 4, 
                  show.legend  = FALSE, 
                  segment.size = 0) + 
  guides(color = guide_legend(nrow = 1)) + 
  # annotate("text", 
  #          x        = as.Date("2021-05-15"), 
  #          y        = 2500, 
  #          label    = "Observed data through\n May 15, 2021", 
  #          size     = 4,
  #          hjust    = 1, 
  #          fontface = "bold",
  #          family   = "Lato") + 
  labs(title    = tmp_title,
       subtitle = tmp_subtitle,
       y        = "Daily deaths",
       x        = "",
       color    = "Date of intervention",
       caption  = glue("**Notes:** Observations and prediction period until May 15, 2021. ",
                       "Figures in boxes show peak number of deaths for each intervention.<br>",
                       "**Abbrev:** CFR, case-fatality rate<br>",
                       "**\uA9 COV-IND-19 Study Group**")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B") +
  theme_classic() +
  theme(
    text            = element_text(family = "Lato"),
    axis.text.x     = element_text(size = 11, vjust = 0.5),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 11, face = "bold"),
    axis.title.y    = element_text(size = 11, face = "bold"),
    legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
    )

ggsave(here("lockdown", "fig", tmp_filename), plot = deaths.p, 
       height = 5.5, width = 11, units = "in", device = cairo_pdf)

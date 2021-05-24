library(tidyverse)
library(plotly)
library(lubridate)
library(ggsci)
library(ggrepel)
library(janitor)
library(glue)
library(here)

mh <- TRUE

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
  filter(date >= "2021-02-15")
# obs <- read_tsv("observed_data.txt")

scenarios <- c("2021-03-01", "2021-03-15", "2021-03-30",
               "2021-04-15", "2021-04-30", "no_intervention")

for (i in seq_along(scenarios)) {
  
  if (mh == TRUE) {
    tmp_filename <- glue("{scenarios[i]}_mh_smooth1_data.txt")
  } else {
    tmp_filename <- glue("{scenarios[i]}_smooth1_data.txt")
  }
  
  if (i == 1) {
    p <- read_tsv(here("lockdown", "data", "revised",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("lockdown", "data", "revised",
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
  )

# mar <- read_tsv("march25_smooth_forecast.txt")

# [1] "March 1"         "March 15"        "March 30"        "April 15"        "April 25"       
# [6] "No intervention"

##march 1
none <- obs %>% 
  filter(date <= "2021-05-02") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(p %>%  
            filter(scenario == "No intervention") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "No intervention") %>%
  mutate(
    smoothed = predict(loess(incidence ~ as.numeric(date), span = 0.5))
  )


##march 1
mar_01 <- obs %>% 
  filter(date <= "2021-03-01") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(p %>%  
            filter(scenario == "March 1") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "March 1") %>%
  mutate(
    smoothed = predict(loess(incidence ~ as.numeric(date), span = 0.5))
  )

##march 15
mar_15 <- obs %>% 
  filter(date <= "2021-03-15") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(p %>%  
            filter(scenario == "March 15") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "March 15") %>%
  mutate(
    smoothed = predict(loess(incidence ~ as.numeric(date), span = 0.5))
  )

##march 30
mar_30 <- obs %>% 
  filter(date <= "2021-03-30") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(p %>%  
            filter(scenario == "March 30") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "March 30") %>%
  mutate(
    smoothed = predict(loess(incidence ~ as.numeric(date), span = 0.5))
  )

##april 15
apr_15 <- obs %>% 
  filter(date <= "2021-04-15") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(p %>%  
            filter(scenario == "April 15") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "April 15")  %>%
  mutate(
    smoothed = predict(loess(incidence ~ as.numeric(date), span = 0.5))
  )

##april 30
apr_30 <- obs %>% 
  filter(date <= "2021-04-30") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(p %>%  
            filter(scenario == "April 30") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "April 30") %>%
  mutate(
    smoothed = predict(loess(incidence ~ as.numeric(date), span = 0.5))
  )

total <- none %>% 
  add_row(mar_01) %>% 
  add_row(mar_15) %>% 
  add_row(mar_30) %>% 
  add_row(apr_15) %>%
  add_row(apr_30) 

ggplotly(total %>% 
  filter(scenario != "March 1") %>% 
  filter(date <= "2021-05-31") %>% 
  nest(-scenario) %>% 
  mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
         fitted = purrr::map(m, `[[`, "fitted")) %>% 
  select(-m) %>% 
  unnest() %>% 
  ggplot(aes(x = date, y = fitted)) + 
  #geom_point() + 
  geom_line(aes(colour = scenario)))


total.smoothed <- total %>% 
  filter(scenario != "March 1") %>% 
  filter(date <= "2021-05-31") %>% 
  nest(-scenario) %>% 
  mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
         fitted = purrr::map(m, `[[`, "fitted")) %>% 
  select(-m) %>% 
  unnest()


total.smoothed.plot <- total.smoothed %>% 
  filter(scenario == "No intervention") %>% 
  filter(date <= "2021-05-19") %>% 
  mutate(scenario = "Observed") %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "No intervention") %>% 
            filter(date >= "2021-05-14")) %>%
  add_row(total.smoothed %>% 
            filter(scenario == "March 15", 
                   date >= "2021-03-08")) %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "March 30", 
                   date >= "2021-03-23")) %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "April 15", 
                   date >= "2021-04-06")) %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "April 30", 
                   date >= "2021-04-21")) %>% 
  mutate(scenario = factor(scenario, levels = c("Observed", "March 15", "March 30", 
                                                "April 15", "April 30", "No intervention")))%>%
  filter(date <= "2021-05-20") 


cases.p <- total.smoothed.plot %>% 
  filter(date >= "2021-02-15" & date <= "2021-05-31") %>%
ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_colour_lancet() + 
  #theme_bw() + 
  xlab("Date") + 
  ylab("Daily cases") + 
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
                        label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
                        color = scenario), 
                   nudge_y = 50000, 
                   nudge_x = -10, 
                   size = 5, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text_repel(data = total.smoothed.plot %>% 
                     group_by(scenario) %>% 
                     filter(date == min(date)) %>% 
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     mutate(text = c("Observed data", "No intervention", "Intervention  1", "Intervention  2", 
                                     "Intervention  3", "Intervention  4"), 
                            x = as.Date(c("2021-03-01", "2021-05-14", "2021-03-08", "2021-03-23", "2021-04-06", "2021-04-16")), 
                            y = c(-10000, 750000,750000,750000,750000,750000)), 
                   aes(x = x, 
                       y = y, 
                       label = text,
                       color = scenario), 
                   nudge_x = -1,
                   size = 5, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  guides(color = guide_legend(nrow = 1)) + 
  annotate("text", 
           x = as.Date("2021-05-02"), 
           y = 430831, 
           label = "Observed data till May 19, 2021.", 
           size = 5, 
           hjust = -0.1, 
           fontface = "bold") + 
  labs(title    = paste0("Predicted number of daily COVID-19 infections for different intervention dates."), 
       subtitle = "Prediction period until May 31, 2021.\nFigures in boxes show peak number of cases for each intervention.",
       y        = "Observed cases",
       x        = "",
       color    = "Date of intervention",
       caption  = "\uA9 COV-IND-19 Study Group") +
  theme_classic() +
  theme(axis.text.x      = element_text(angle = 45, vjust = 0.5, size = 14, face = "bold"),
        axis.text.y      = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 14, face = "bold"),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14,hjust = 0, color = "gray40"),
        plot.caption = element_text(size = 14,hjust = 0)) +
  scale_y_continuous(labels = scales::comma)
  
if (mh == TRUE) {
  tmp_filename <- "cases_mh.pdf"
} else {
  cfr <- cfr %>% filter(place == "India")
  tmp_filename <- "cases.pdf"
}  
  
ggsave(here("lockdown", "fig", tmp_filename), plot = cases.p, height = 12, width = 18, units = "in")



  
  

  
    




obs <- read_tsv("observed_data.txt") %>% mutate(cfr = daily_deaths/daily_cases)
mar <- read_tsv("march25_smooth_forecast.txt")
cfr <- read_tsv("march25_cfr.txt")



# [1] "March 1"         "March 15"        "March 30"        "April 15"        "April 25"       
# [6] "No intervention"

##march 1
mar.01 <- obs %>% 
  filter(date <= "2021-03-01") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(mar %>%  
            filter(scenario == "March 1") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(obs %>% select(date, cfr)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "March 1") 

#march 15
mar.15 <- obs %>% 
  filter(date <= "2021-03-15") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(mar %>%  
            filter(scenario == "March 15") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(obs %>% select(date, cfr)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "March 15") %>% 
  drop_na()


##march 30
mar.30 <- obs %>% 
  filter(date <= "2021-03-30") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(mar %>%  
            filter(scenario == "March 30") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(obs %>% select(date, cfr)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "March 30")%>% 
  drop_na()



##april 15
apr.15 <- obs %>% 
  filter(date <= "2021-04-15") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(mar %>%  
            filter(scenario == "April 15") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(obs %>% select(date, cfr)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "April 15")%>% 
  drop_na()

##april 25
apr.25 <- obs %>% 
  filter(date <= "2021-04-25") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_row(mar %>%  
            filter(scenario == "April 25") %>% 
            select(date, incidence) %>% 
            drop_na() %>% 
            left_join(obs %>% select(date, cfr)) %>% 
            mutate(incidence = incidence*cfr) %>% 
            select(-cfr)) %>% 
  add_column(scenario = "April 25")%>% 
  drop_na()


total <- obs %>% 
  filter(date <= "2021-05-02") %>% 
  select(date, daily_deaths) %>% 
  rename(incidence = daily_deaths) %>% 
  add_column(scenario = "Observed") %>% 
  add_row(mar.01) %>% 
  add_row(mar.15) %>% 
  add_row(mar.30) %>% 
  add_row(apr.15) %>%
  add_row(apr.25) 

ggplotly(total %>% 
           filter(scenario != "March 1") %>%
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
  filter(date <= "2021-05-03") %>% 
  nest(-scenario) %>% 
  mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
         fitted = purrr::map(m, `[[`, "fitted")) %>% 
  select(-m) %>% 
  unnest()


total.smoothed.plot <- total.smoothed %>% 
  filter(scenario == "Observed") %>% 
  filter(date <= "2021-05-02")  %>% 
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
                   date >= "2021-04-09")) %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "April 25", 
                   date >= "2021-04-18")) %>% 
  mutate(scenario = factor(scenario, levels = c("Observed", "March 15", "March 30", 
                                                "April 15", "April 25")))%>%
  filter(date <= "2021-05-03") 



deaths.p <- total.smoothed.plot %>% 
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_colour_lancet() + 
  #theme_bw() + 
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
                       color = scenario), 
                   nudge_y = 500, 
                   nudge_x = -10, 
                   size = 5, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text_repel(data = total.smoothed.plot %>% 
                    group_by(scenario) %>% 
                    filter(date == min(date)) %>% 
                    dplyr::ungroup() %>% 
                    select(scenario, date, fitted) %>% 
                    mutate(text = c("Observed data", "Intervention  1", "Intervention  2", 
                                    "Intervention  3", "Intervention  4"), 
                           x = as.Date(c("2021-03-01", "2021-03-10", "2021-03-25", "2021-04-10", "2021-04-19")), 
                           y = c(0, 3500,3500,3500,3500)), 
                  aes(x = x, 
                      y = y, 
                      label = text,
                      color = scenario), 
                  nudge_x = -5,
                  size = 5, 
                  show.legend  = FALSE, 
                  segment.size = 0) + 
  guides(color = guide_legend(nrow = 1)) + 
  annotate("text", 
           x = as.Date("2021-04-20"), 
           y = 3750, 
           label = "Observed data till\n May 2, 2021.", 
           size = 5, 
           hjust = -0.1, 
           fontface = "bold") + 
  labs(title    = paste0("Predicted number of daily COVID-19 deaths for different intervention dates."), 
       subtitle = "Observations and prediction period until May 2, 2021.\nFigures in boxes show peak number of deaths for each intervention.",
       y        = "Observed deaths",
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

ggsave("deaths.pdf", plot = deaths.p, height = 12, width = 18, units = "in")

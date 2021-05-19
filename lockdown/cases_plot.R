require(tidyverse)
require(plotly)
require(lubridate)
require(ggsci)
require(ggrepel)

obs <- read_tsv("observed_data.txt")
mar <- read_tsv("march25_smooth_forecast.txt")

# [1] "March 1"         "March 15"        "March 30"        "April 15"        "April 25"       
# [6] "No intervention"

##march 1
none <- obs %>% 
  filter(date <= "2021-05-02") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(mar %>%  
            filter(scenario == "No intervention") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "No intervention")


##march 1
mar.01 <- obs %>% 
  filter(date <= "2021-03-01") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(mar %>%  
            filter(scenario == "March 1") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "March 1")

##march 15
mar.15 <- obs %>% 
  filter(date <= "2021-03-15") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(mar %>%  
            filter(scenario == "March 15") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "March 15")

##march 30
mar.30 <- obs %>% 
  filter(date <= "2021-03-30") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(mar %>%  
            filter(scenario == "March 30") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "March 30")

##april 15
apr.15 <- obs %>% 
  filter(date <= "2021-04-15") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(mar %>%  
            filter(scenario == "April 15") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "April 15")

##april 15
apr.25 <- obs %>% 
  filter(date <= "2021-04-25") %>% 
  select(date, daily_cases) %>% 
  rename(incidence = daily_cases) %>% 
  add_row(mar %>%  
            filter(scenario == "April 25") %>% 
            select(date, incidence) %>% 
            drop_na()) %>% 
  add_column(scenario = "April 25")

total <- none %>% 
  add_row(mar.01) %>% 
  add_row(mar.15) %>% 
  add_row(mar.30) %>% 
  add_row(apr.15) %>%
  add_row(apr.25) 

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
  filter(date <= "2021-05-02") %>% 
  mutate(scenario = "Observed") %>% 
  add_row(total.smoothed %>% 
            filter(scenario == "No intervention") %>% 
            filter(date >= "2021-05-02")) %>%
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
            filter(scenario == "April 25", 
                   date >= "2021-04-16")) %>% 
  mutate(scenario = factor(scenario, levels = c("Observed", "March 15", "March 30", 
                                                "April 15", "April 25", "No intervention")))%>%
  filter(date <= "2021-05-20") 


cases.p <- total.smoothed.plot %>% 
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
           label = "Observed data till May 2, 2021.", 
           size = 5, 
           hjust = -0.1, 
           fontface = "bold") + 
  labs(title    = paste0("Predicted number of daily COVID-19 infections for different intervention dates."), 
       subtitle = "Prediction period until May 20, 2021.\nFigures in boxes show peak number of cases for each intervention.",
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
  
  
  
ggsave("cases.pdf", plot = cases.p, height = 12, width = 18, units = "in")



  
  

  
    




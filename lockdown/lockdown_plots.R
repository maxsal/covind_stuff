library(tidyverse)
library(glue)
library(here)


today <- Sys.Date() - 2
d <- c("2021-03-01", "2021-03-15", "2021-03-30", "2021-04-15", "2021-04-25", "no_intervention")

for (i in seq_along(d)) {
  
  if (i == 1) {
    dat <- read_tsv(here("lockdown", "data", glue("{d[i]}_smooth1_data.txt")),
                    col_types = cols()) %>% mutate(scenario = d[i])
  } else {
    dat <- bind_rows(dat, 
                     read_tsv(here("lockdown", "data", glue("{d[i]}_smooth1_data.txt")),
                              col_types = cols()) %>% mutate(scenario = d[i])
                     )
  }
  
}

obs <- read_csv(glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{today}/everything.csv"),
                col_types = cols()) %>%
  filter(place == "India" & date >= "2021-02-15") %>%
  mutate(scenario = "Observed")

cols <- c(
  "Observed"        = "black",
  "March 1"         = viridis::viridis(5)[1],
  "March 15"        = viridis::viridis(5)[1],
  "March 30"        = viridis::viridis(5)[2],
  "April 15"        = viridis::viridis(5)[3],
  "April 25"        = viridis::viridis(5)[4],
  "No intervention" = "red"
)

dat %>%
  mutate(
    scenario = case_when(
      scenario == "2021-03-01" ~ "March 1",
      scenario == "2021-03-15" ~ "March 15",
      scenario == "2021-03-30" ~ "March 30",
      scenario == "2021-04-15" ~ "April 15",
      scenario == "2021-04-25" ~ "April 25",
      scenario == "no_intervention" ~ "No intervention"
    )
  ) %>%
  filter(!(scenario %in% c("No intervention")) & date >= start_date) %>%
  ggplot(aes(x = date, y = value, color = scenario, group = scenario)) +
  geom_line(data = obs, aes(x = date, y = cases), size = 1.2) +
  geom_line(size = 1.2) +
  labs(
    title = "Total case counts under lockdown",
    x     = "Date",
    y     = "Total case count"
  ) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave(here("lockdown", "fig", "total_case_lockdown_june_smooth1.pdf"), width = 7, height = 5, device = cairo_pdf)

start_date <- "2021-03-01"

plt2 <- dat %>%
  mutate(
    scenario = case_when(
      scenario == "2021-03-01" ~ "March 1",
      scenario == "2021-03-15" ~ "March 15",
      scenario == "2021-03-30" ~ "March 30",
      scenario == "2021-04-15" ~ "April 15",
      scenario == "2021-04-25" ~ "April 25",
      scenario == "no_intervention" ~ "No intervention"
    )
  ) %>%
  filter(date >= start_date & !(scenario %in% c("No intervention", "March 1"))) %>%
  ggplot(aes(x = date, y = incidence, color = scenario, group = scenario)) +
  geom_vline(xintercept = as.Date("2021-03-15"), size = 1, linetype = 2, color = cols[3]) +
  geom_vline(xintercept = as.Date("2021-03-30"), size = 1, linetype = 2, color = cols[4]) +
  geom_vline(xintercept = as.Date("2021-04-15"), size = 1, linetype = 2, color = cols[5]) +
  geom_vline(xintercept = as.Date("2021-04-25"), size = 1, linetype = 2, color = cols[6]) +
  geom_line(data = obs %>% filter(date >= start_date), aes(x = date, y = daily_cases), size = 1.2) +
  # geom_smooth(data = obs %>% filter(date >= start_date), aes(x = date, y = daily_cases),
  #             method = "loess", formula = "y ~  x", se = FALSE, span = 0.3, size = 1.2) +
  # geom_smooth(method = "loess", formula = "y ~  x", se = FALSE, span = 0.3,size = 1.2) +
  geom_line(size = 1.2) +
  labs(
    title = "Daily case counts under lockdown",
    x     = "Date",
    y     = "Daily case counts"
  ) +
  scale_color_manual(values = cols) +
  scale_y_continuous(labels = scales::comma) +
  theme_classic() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "top",
    legend.title = element_blank()
  )
plt2
ggsave(here("lockdown", "fig", "daily_case_lockdown_june_smooth1.pdf"), width = 7, height = 5, device = cairo_pdf)

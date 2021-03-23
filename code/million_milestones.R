library(tidyverse)
library(janitor)
library(glue)
library(ggtext)
library(extrafont)

# jhu data -----------
jhu <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                col_types = cols()) %>%
  rename(
    country = `Country/Region`
  )

tidy_country <- function(data, place) {
  data %>% 
    filter(country == place) %>%
    select(-c(`Province/State`, Lat, Long)) %>%
    pivot_longer(
      names_to  = "date",
      values_to = "count",
      -country
    ) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(count != 0)
}

# us and brazil ----------
us  <- jhu %>% tidy_country("US")

brazil <- jhu %>% tidy_country("Brazil")

russia <- jhu %>% tidy_country("Russia")


bangladesh <- jhu %>% tidy_country("Bangladesh")
pakistan   <- jhu %>% tidy_country("Pakistan")

# india ----------
india <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                  col_types = cols()) %>%
  clean_names() %>%
  mutate(
    date    = as.Date(date, "%d %B"),
    country = "India"
  ) %>%
  select(country, date, count = total_confirmed) %>%
  drop_na(date, count)

# combine ----------
us_min <- us %>% filter(date == min(date))
us_mil <- us %>% filter(count >= 1000000) %>% filter(date == min(date))
us_2mil <- us %>% filter(count >= 2000000) %>% filter(date == min(date))
us_3mil <- us %>% filter(count >= 3000000) %>% filter(date == min(date))
us_4mil <- us %>% filter(count >= 4000000) %>% filter(date == min(date))
us_5mil <- us %>% filter(count >= 5000000) %>% filter(date == min(date))
us_6mil <- us %>% filter(count >= 6000000) %>% filter(date == min(date))
us_7mil <- us %>% filter(count >= 7000000) %>% filter(date == min(date))
us_8mil <- us %>% filter(count >= 8000000) %>% filter(date == min(date))

brazil_min <- brazil %>% filter(date == min(date))
brazil_mil <- brazil %>% filter(count >= 1000000) %>% filter(date == min(date))
brazil_2mil <- brazil %>% filter(count >= 2000000) %>% filter(date == min(date))
brazil_3mil <- brazil %>% filter(count >= 3000000) %>% filter(date == min(date))
brazil_4mil <- brazil %>% filter(count >= 4000000) %>% filter(date == min(date))
brazil_5mil <- brazil %>% filter(count >= 5000000) %>% filter(date == min(date))

india_min <- india %>% filter(date == min(date))
india_mil <- india %>% filter(count >= 1000000) %>% filter(date == min(date))
india_2mil <- india %>% filter(count >= 2000000) %>% filter(date == min(date))
india_3mil <- india %>% filter(count >= 3000000) %>% filter(date == min(date))
india_4mil <- india %>% filter(count >= 4000000) %>% filter(date == min(date))
india_5mil <- india %>% filter(count >= 5000000) %>% filter(date == min(date))
india_6mil <- india %>% filter(count >= 6000000) %>% filter(date == min(date))
india_7mil <- india %>% filter(count >= 7000000) %>% filter(date == min(date))
# india_8mil <- india %>% filter(count >= 8000000) %>% filter(date == min(date))
india_8mil <- tibble(country = "India", date = as.Date("2020-10-28"), count = 8038765)

russia_min <- russia %>% filter(date == min(date))
russia_mil <- russia %>% filter(count >= 1000000) %>% filter(date == min(date))

us_milestones <- tibble(
  country = rep("US", 8),
  days    = c(
    us_mil %>% pull(date) - us_min %>% pull(date),
    us_2mil %>% pull(date) - us_mil %>% pull(date),
    us_3mil %>% pull(date) - us_2mil %>% pull(date),
    us_4mil %>% pull(date) - us_3mil %>% pull(date),
    us_5mil %>% pull(date) - us_4mil %>% pull(date),
    us_6mil %>% pull(date) - us_5mil %>% pull(date),
    us_7mil %>% pull(date) - us_6mil %>% pull(date),
    us_8mil %>% pull(date) - us_7mil %>% pull(date)
    ),
  milestone = c("0 to 1M", "1M to 2M", "2M to 3M", "3M to 4M", "4M to 5M", "5M to 6M", "6M to 7M", "7M to 8M"))

brazil_milestones <- tibble(
  country = rep("Brazil", 5),
  days    = c(
    brazil_mil %>% pull(date) - brazil_min %>% pull(date),
    brazil_2mil %>% pull(date) - brazil_mil %>% pull(date),
    brazil_3mil %>% pull(date) - brazil_2mil %>% pull(date),
    brazil_4mil %>% pull(date) - brazil_3mil %>% pull(date),
    brazil_5mil %>% pull(date) - brazil_4mil %>% pull(date)
  ),
  milestone = c("0 to 1M", "1M to 2M", "2M to 3M", "3M to 4M", "4M to 5M"))

india_milestones <- tibble(
  country = rep("India", 8),
  days    = c(
    india_mil %>% pull(date) - india_min %>% pull(date),
    india_2mil %>% pull(date) - india_mil %>% pull(date),
    india_3mil %>% pull(date) - india_2mil %>% pull(date),
    india_4mil %>% pull(date) - india_3mil %>% pull(date),
    india_5mil %>% pull(date) - india_4mil %>% pull(date),
    india_6mil %>% pull(date) - india_5mil %>% pull(date),
    india_7mil %>% pull(date) - india_6mil %>% pull(date),
    india_8mil %>% pull(date) - india_7mil %>% pull(date)
    ),
  milestone = c("0 to 1M", "1M to 2M", "2M to 3M", "3M to 4M", "4M to 5M", "5M to 6M", "6M to 7M", "7M to 8M"))

russia_milestones <- tibble(
  country = rep("Russia", 1),
  days    = c(
    russia_mil %>% pull(date) - russia_min %>% pull(date)
  ),
  milestone = c("0 to 1M"))

# stack -----------
milestones <- bind_rows(
  us_milestones,
  brazil_milestones,
  # russia_milestones,
  india_milestones
) %>%
  mutate(
    milestone = as.factor(milestone),
    country   = factor(country, levels = c("Brazil", "US", "Russia", "India"))
    )

# plot ----------
clrs <- c("US" = "#173F5F", "Brazil" = "#f2c82e", "India" = "#ED553B", "Russia" = "#009E73")

milestones %>%
  ggplot(aes(x = milestone, y = as.numeric(days), group = country)) +
  geom_bar(aes(fill = country), position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity") +
  geom_text(aes(label=as.numeric(days), color = country), family = "Lato", fontface = "bold", position = position_dodge2(width = 0.9, preserve = "single"), hjust = -0.3) +
  labs(
    title    = "How quick are countries hitting every million COVID-19 cases?",
    # subtitle = "COVID-19",
    x        = "Milestone",
    y        = "Days",
    caption  = glue("**\uA9 COV-IND-19 Study Group**<br>",
                    "**Brazil & US source:** JHU CSSE GitHub<br>",
                    "**India source:** covid19india.org")
  ) +
  scale_fill_manual(values = clrs) +
  scale_color_manual(values = clrs) +
  scale_x_discrete(limits = rev(levels(milestones$milestone))) +
  coord_flip() +
  theme_minimal() +
  theme(
    text               = element_text(family = "Lato"),
    plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle      = element_text(size = 14, color = "#36454f"),
    plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
    axis.text          = element_text(size = 10, color = "#36454f"),
    axis.title         = element_text(size = 12, face = "italic"),
    legend.position    = "top",
    legend.title       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave("~/Downloads/million_milestones.pdf", width = 8.55, height = 7, device = cairo_pdf)
ggsave("~/Downloads/million_milestones.png", width = 8.55, height = 7, dpi = "retina")


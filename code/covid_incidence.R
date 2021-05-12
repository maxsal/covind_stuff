source("libraries.R")
source("functions.R")

# jhu data -----------
jhu <- get_jhu_data()

us     <- jhu %>% tidy_country("US")
russia <- jhu %>% tidy_country("Russia")
france <- jhu %>% tidy_country("France")
italy  <- jhu %>% tidy_country("Italy")
uk     <- jhu %>% tidy_country("United Kingdom")
brazil <- jhu %>% tidy_country("Brazil")

india <- get_covid19india_data()

d <- bind_rows(us, russia, italy, france, uk, brazil, india) %>%
  drop_na()

euro <- c("France", "Italy", "Russia", "United Kingdom")
noneuro <- c("US", "Brazil", "India")

d %>%
  plot_cov_inc_by_country(countries = euro)

ggsave(here("fig", "covid_incidence_by_euro.pdf"),
       width = 8, height = 5,
       device = cairo_pdf)

d %>%
  plot_cov_inc_by_country(countries = noneuro)

ggsave(here("fig", "covid_incidence_by_noneuro.pdf"),
       width = 8, height = 5,
       device = cairo_pdf)

# deaths -----------
jhu_d <- get_jhu_data(path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

us_d     <- jhu_d %>% tidy_country("US")
russia_d <- jhu_d %>% tidy_country("Russia")
france_d <- jhu_d %>% tidy_country("France")
italy_d  <- jhu_d %>% tidy_country("Italy")
uk_d     <- jhu_d %>% tidy_country("United Kingdom")
brazil_d <- jhu_d %>% tidy_country("Brazil")

india_d <- get_covid19india_data(death = TRUE)

d_d <- bind_rows(us_d, russia_d, italy_d, france_d, uk_d, brazil_d, india_d) %>%
  drop_na()

d_d %>%
  plot_cov_inc_by_country(countries = euro,
                          title = "Daily COVID-19 deaths by country",
                          y_axis = "Daily deaths")
ggsave(here("fig", "covid_death_by_euro.pdf"),
       width = 8, height = 5,
       device = cairo_pdf)

d_d %>%
  plot_cov_inc_by_country(countries = noneuro,
                          title = "Daily COVID-19 deaths by country",
                          y_axis = "Daily deaths")
ggsave(here("fig", "covid_death_by_noneuro.pdf"),
       width = 8, height = 5,
       device = cairo_pdf)

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

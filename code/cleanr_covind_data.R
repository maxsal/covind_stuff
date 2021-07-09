librarian::shelf(
  tidyverse, EpiEstim, gt, glue, lubridate, janitor, scales, ggtext, here, httr,
  maxsal/covid19india
)

source(here("code", "functions", "functions.R"))

set_seed <- 46342
set.seed(set_seed)

dat <- get_all_data()

cfr    <- get_cfr(dat) %>% distinct()
r0_est <- get_r_est(dat)
tabs   <- get_metrics_tables()

tabs$full
tabs$point_in_time
tabs$cumulative

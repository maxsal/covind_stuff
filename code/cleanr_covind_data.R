pacman::p_load(
  tidyverse, EpiEstim, gt, glue, lubridate, janitor, scales, ggtext, here, httr
)
source(here("code", "functions", "functions.R"))

set_seed <- 46342
set.seed(set_seed)

dat    <- do_it_all()
cfr    <- get_cfr(dat)
r0_est <- get_r_est(dat)
tabs   <- get_metrics_tables()

tabs$full
tabs$point_in_time
tabs$cumulative

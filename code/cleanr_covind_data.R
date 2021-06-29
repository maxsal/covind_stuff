librarian::shelf(
  tidyverse, EpiEstim, gt, glue, lubridate, janitor, scales, ggtext, here, httr,
  maxsal/covid19india
)

source(here("code", "functions", "functions.R"))

set_seed <- 46342
set.seed(set_seed)

dat <- bind_rows(get_nat_counts(), get_state_counts()) %>%
  left_join(
    bind_rows(
      get_nat_tests(),
      get_state_tests()
      ),
    by = c("place", "date")) %>%
  left_join(
    get_r0(.) %>%
      dplyr::rename(
        r_est   = r,
        r_lower = lower,
        r_upper = upper
        ), 
    by = c("place", "date")) %>%
  left_join(
    pop %>%
      dplyr::select(-population),
    by = "place") %>%
  mutate(tpr = daily_cases / daily_tests)

cfr    <- get_cfr(dat) %>% distinct()
r0_est <- get_r_est(dat)
tabs   <- get_metrics_tables()

tabs$full
tabs$point_in_time
tabs$cumulative

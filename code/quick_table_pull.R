pacman::p_load(tidyverse, glue, here, janitor, zoo)

source(here("lockdown", "period_summary.R"))

cfr_sched <- "kl"
mh        <- TRUE
wane      <- TRUE

cfr_type  <- ifelse(cfr_sched == "india", "mod", ifelse(cfr_sched == "mh", "high", ifelse(cfr_sched == "kl", "low", NA)))
le_type   <- ifelse(mh == TRUE, "mod", ifelse(mh == FALSE, "strong", NA))
wane_type <- ifelse(wane == TRUE, "wane", "main")

mar_01_mar_15 <- period_summary(scen = "2021-03-01", end_date = "2021-03-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_01_mar_30 <- period_summary(scen = "2021-03-01", end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_01_apr_15 <- period_summary(scen = "2021-03-01", end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_01_apr_30 <- period_summary(scen = "2021-03-01", end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_01_may_15 <- period_summary(scen = "2021-03-01", end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_15_mar_30 <- period_summary(scen = "2021-03-15", end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_15_apr_15 <- period_summary(scen = "2021-03-15", end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_15_apr_30 <- period_summary(scen = "2021-03-15", end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_15_may_15 <- period_summary(scen = "2021-03-15", end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_30_apr_15 <- period_summary(scen = "2021-03-30", end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_30_apr_30 <- period_summary(scen = "2021-03-30", end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_30_may_15 <- period_summary(scen = "2021-03-30", end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
apr_15_apr_30 <- period_summary(scen = "2021-04-15", end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
apr_15_may_15 <- period_summary(scen = "2021-04-15", end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

print_cases <- function(x) {
  
  a <- x$observed_period_cases
  b <- x$predicted_period_cases
  d <- x$pred_cases_averted
  f <- x$pct_cases_averted
  
  glue("
       {format(a, nsmall = 1)}
       {format(b[1], nsmall = 1)} [{format(b[2], nsmall = 1)}, {format(b[3], nsmall = 1)}]
       {format(d[1], nsmall = 1)} [{format(d[2], nsmall = 1)}, {format(d[3], nsmall = 1)}]
       {format(f[1], nsmall = 1)}% [{format(f[2], nsmall = 1)}%, {format(f[3], nsmall = 1)}%]
       ")
  
}

cases_out <- tibble(
  "March 1" = c(print_cases(mar_01_mar_15), print_cases(mar_01_mar_30), print_cases(mar_01_apr_15), print_cases(mar_01_apr_30), print_cases(mar_01_may_15)),
  "March 15" = c("-", print_cases(mar_15_mar_30), print_cases(mar_15_apr_15), print_cases(mar_15_apr_30), print_cases(mar_15_may_15)),
  "March 30" = c("-", "-", print_cases(mar_30_apr_15), print_cases(mar_30_apr_30), print_cases(mar_30_may_15)),
  "April 15" = c("-", "-", "-", print_cases(apr_15_apr_30), print_cases(apr_15_may_15))
)

write_tsv(cases_out, glue("~/Downloads/{le_type}le_{cfr_type}cfr_{wane_type}_cases.txt"))
# as_tibble(c(print_cases(mar_01_mar_30), print_cases(mar_15_mar_30), "-", "-"))
# print_cases(mar_01_apr_15)
# print_cases(mar_01_apr_30)
# print_cases(mar_01_may_15)
# print_cases(mar_15_apr_15)
# print_cases(mar_15_apr_30)
# print_cases(mar_15_may_15)
# print_cases(mar_30_apr_15)
# print_cases(mar_30_apr_30)
# print_cases(mar_30_may_15)
# print_cases(apr_15_apr_30)
# print_cases(apr_15_may_15)

print_deaths <- function(x) {
  
  a <- x$observed_period_deaths
  b <- x$predicted_period_deaths
  d <- x$pred_deaths_averted
  f <- x$pct_deaths_averted
  
  glue("
       {format(a, nsmall = 1)}
       {format(b[1], nsmall = 1)} [{format(b[2], nsmall = 1)}, {format(b[3], nsmall = 1)}]
       {format(d[1], nsmall = 1)} [{format(d[2], nsmall = 1)}, {format(d[3], nsmall = 1)}]
       {format(f[1], nsmall = 1)}% [{format(f[2], nsmall = 1)}%, {format(f[3], nsmall = 1)}%]
       ")
  
}

deaths_out <- tibble(
  "March 1" = c(print_deaths(mar_01_mar_15), print_deaths(mar_01_mar_30), print_deaths(mar_01_apr_15), print_deaths(mar_01_apr_30), print_deaths(mar_01_may_15)),
  "March 15" = c("-", print_deaths(mar_15_mar_30), print_deaths(mar_15_apr_15), print_deaths(mar_15_apr_30), print_deaths(mar_15_may_15)),
  "March 30" = c("-", "-", print_deaths(mar_30_apr_15), print_deaths(mar_30_apr_30), print_deaths(mar_30_may_15)),
  "April 15" = c("-", "-", "-", print_deaths(apr_15_apr_30), print_deaths(apr_15_may_15))
)
write_tsv(deaths_out, glue("~/Downloads/{le_type}le_{cfr_type}cfr_{wane_type}_deaths.txt"))

# print_deaths(mar_01_mar_15)
# print_deaths(mar_01_mar_30)
# print_deaths(mar_01_apr_15)
# print_deaths(mar_01_apr_30)
# print_deaths(mar_01_may_15)
# print_deaths(mar_15_mar_30)
# print_deaths(mar_15_apr_15)
# print_deaths(mar_15_apr_30)
# print_deaths(mar_15_may_15)
# print_deaths(mar_30_apr_15)
# print_deaths(mar_30_apr_30)
# print_deaths(mar_30_may_15)
# print_deaths(apr_15_apr_30)
# print_deaths(apr_15_may_15)

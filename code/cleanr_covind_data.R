library(tidyverse)
library(janitor)
library(glue)
library(EpiEstim)
library(ggtext)
library(here)
source(here("code", "functions", "functions.R"))
# source("scripts/functions.R")

set_seed <- 46342
set.seed(set_seed)

d      <- "2021-05-23"
thresh <- "2020-05-31"

dat <- do_it_all(d = d)

# abbrevs <- get_abbrevs(dat)
# 
# f_dat <- get_forecast_data(d = d, ab = abbrevs)

cfr <- get_cfr(dat)

r0_est <- get_r_est(dat)

# write_csv(dat, here::here("data", "results", glue::glue("everything_{format(as.Date(thresh), '%Y%m%d')}.csv")))
# write_csv(cfr, here::here("data", "results", glue::glue("cfr_{format(as.Date(thresh), '%Y%m%d')}.csv")))
# write_csv(r0_est, here::here("data", "results", glue::glue("r0_est_{format(as.Date(thresh), '%Y%m%d')}.csv")))

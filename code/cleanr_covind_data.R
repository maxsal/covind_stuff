suppressPackageStartupMessages({
  library(tidyverse)
  library(EpiEstim)
  library(gt)
  library(glue)
  library(lubridate)
  library(janitor)
  library(scales)
  library(data.table)
  library(vroom)
  library(ggtext)
  library(here)
  library(httr)
})
source(here("code", "functions", "functions.R"))
# source("scripts/functions.R")

set_seed <- 46342
set.seed(set_seed)

d      <- "2021-05-23"
thresh <- "2020-05-31"

dat <- do_it_all(d = d)

cfr <- get_cfr(dat)

r0_est <- get_r_est(dat)

tabs <- India_gt_table()

tabs$full
tabs$point_in_time
tabs$cumulative

ally::libri(
  tidyverse, janitor, glue, ggtext, extrafont, here, httr, patchwork,
  EpiEstim, maxsal/covid19india, data.table, scales 
)

source("functions.R")

set_seed <- 46342
set.seed(set_seed)

source("code/make_stack_plots.R")
# source(here("code", "make_test_stack_plots.R"))
source("code/make_vax_bar_plots.R")
# source(here("code", "india_vs_wout_state.R"))
# source(here("code", "peak_comparison.R"))

tabs <- covid19india::get_metrics_tables()

tabs$full
tabs$point_in_time
tabs$cumulative

source("code/clean_repo.R")

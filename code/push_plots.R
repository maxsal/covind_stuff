source(here::here("code", "functions", "libri.R"))
libri(
  tidyverse, janitor, glue, ggtext, extrafont, here, httr, patchwork,
  EpiEstim, maxsal/covid19india
)
source(here("functions.R"))

source(here("code", "make_stack_plots.R"))
source(here("code", "make_test_stack_plots.R"))
source(here("code", "make_metrics_tables.R"))

tabs$full
tabs$point_in_time
tabs$cumulative

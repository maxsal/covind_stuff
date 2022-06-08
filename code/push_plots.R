ally::libri(
  tidyverse, janitor, glue, ggtext, extrafont, here, httr, patchwork,
  EpiEstim, maxsal/covid19india, data.table, scales, git2r, gt, rprojroot 
)

source("functions.R")

set_seed <- 46342
set.seed(set_seed)

source("code/make_plots.R")

tabs <- covid19india::get_metrics_tables()

quick_save(x = tabs, date = vax_max_date)

source("code/clean_repo.R")

quick_commit()


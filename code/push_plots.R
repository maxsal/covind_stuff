ally::libri(
  tidyverse, janitor, glue, ggtext, extrafont, here, httr, patchwork,
  EpiEstim, maxsal/covid19india, data.table, scales, git2r 
)

source("functions.R")

set_seed <- 46342
set.seed(set_seed)

source("code/make_plots.R")

tabs <- covid19india::get_metrics_tables()

tabs$full
tabs$point_in_time
tabs$cumulative

source("code/clean_repo.R")

git2r::add(repo = ".", path = ".", force = FALSE)
git2r::commit(repo = ".", message = "push plots")
git2r::push(name = "origin", refspec = "refs/heads/main")
git2r::push(name = "gitlab", refspec = "refs/heads/main")

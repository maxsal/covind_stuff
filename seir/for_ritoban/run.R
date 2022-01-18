# libraries -------------
library(data.table)
library(tidyverse)
library(patchwork)

# specs -----------
max_date <- as.Date("2022-01-11") # last date of training period
obs_days <- 98                    # days of training period
t_pred   <- 50                    # predicition days

# source scripts -----------
scripts <- c("clean_prediction.R", "get_impo.R", "get_init.R", "get_phase.R", 
             "mcmc_performR.r", "model_deterministic_simulateR.r", "model_estimateR.r", 
             "model_initializeR.r", "model_plotR.r", "model_predictR.r", "model_stochastic_simulateR.r", 
             "par_initializeR.r", "R0_calculateR.r")
for (i in seq_along(scripts)) {
  source(paste0("scripts/", scripts[i]))
}

# load Result outputs -----------
r8  <- readRDS("model_output/Result8.rds")
r10 <- readRDS("model_output/Result10.rds")
r14 <- readRDS("model_output/Result14.rds")

# clean up result output ----------
quick_proc <- function(x) {
  clean_prediction(
    x,
    state = "India",
    obs_days = obs_days, t_pred = t_pred,
    obs_dates = seq.Date(from = Sys.Date() - obs_days, to = Sys.Date() - 1, by = "day")
  ) %>%
    as.data.table()
}

clean_r8 <- quick_proc(x = r8$prediction)
clean_r10 <- quick_proc(x = r10$prediction)
clean_r14 <- quick_proc(x = r14$prediction)

# plots ------------

# reported daily cases
(rep_cases <- plot_daily_reported_cases())

# unreported daily cases
(unrep_cases <- plot_daily_unreported_cases())

# reported daily deaths
(rep_deaths <- plot_daily_reported_deaths())

# unreported daily deaths
(unrep_deaths <- plot_daily_unreported_deaths())

# patch
patched <- (rep_cases + rep_deaths) / (unrep_cases + unrep_deaths)
ggsave(plot = patched, filename = "plots/case_death_grid.pdf",
       width = 16, height = 10, device = cairo_pdf)

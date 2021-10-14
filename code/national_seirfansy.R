suppressPackageStartupMessages({
  library(covid19india)
  library(data.table)
  library(tidyverse)
  library(magrittr)
  library(arm)
  library(janitor)
  library(DescTools)
  library(patchwork)
  library(pbapply)
  library(SEIRfansy)
})

f <- c("clean_prediction.R", "get_impo.R", "get_init.R", "get_phase.R")
for (i in seq_along(f)) {source(paste0("code/functions/", f[i]))}

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
  n_iter    <- 1e5
  burn_in   <- 1e5
  opt_num   <- 200
} else {
  n_iter    <- 1e3 #default 1e5
  burn_in   <- 1e2 #default 1e5
  opt_num   <- 20  #default 200
}

# specs -----------
today      <- Sys.Date()
state      <- "tt"
state_name <- "India"

t_pred     <- 150 # number of predicted days
N          <- covid19india::pop %>% filter(abbrev == state) %>% pull(population)
plt        <- FALSE
save_plt   <- FALSE

# load and prepare ----------
data     <- get_nat_counts()[, .(date, Confirmed = daily_cases, Recovered = daily_recovered, Deceased = daily_deaths)]
max_date <- min(data[, max(date)], Sys.Date() - 1)
message("max date: ", max_date)
data     <- data[date <= max_date]
min_date <- as.Date(max_date - 99)
obs_days <- length(as.Date(min_date):as.Date(max_date))

data_initial <- get_init(data)

data <- data[date >= min_date]

mCFR <- tail(cumsum(data$Deceased) / cumsum(data$Deceased + data$Recovered), 1)

phases <- get_phase(start_date = min_date, end_date = max_date)

tryCatch(
  expr = {
    # predict -----------
    result    <- SEIRfansy.predict(
      data            = abs(data[, !c("date")]),
      init_pars       = NULL,
      data_init       = data_initial,
      T_predict       = t_pred,
      niter           = n_iter,
      BurnIn          = burn_in,
      model           = "Multinomial",
      N               = N,
      lambda          = 1/(69.416 * 365),
      mu              = 1/(69.416 * 365),
      period_start    = phases,
      opt_num         = opt_num,
      auto.initialize = T,
      alpha_u         = 0.5,
      f               = 0.15,
      plot            = plt,
      save_plots      = save_plt
    )
    
    # directory ----------
    wd <- paste0(data_repo, "/", today, "/seirfansy")
    if (!dir.exists(wd)) {
      dir.create(wd, recursive = TRUE)
      message("Creating ", wd)
    }
    
    pred_clean <- clean_prediction(result$prediction,
                                   state    = state_name,
                                   obs_days = obs_days,
                                   t_pred   = t_pred)
    
    write_tsv(pred_clean, paste0(wd, "/prediction_", state, "_", max_date, ".txt"))
    write_tsv(as_tibble(result$mcmc_pars, .name_repair = "unique"), paste0(wd, "/prediction_pars_", state, "_", max_date, ".txt"))
    
    p_pred <- pred_clean %>%
      filter(section == "positive_reported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    
    r_pred <- pred_clean %>%
      filter(section == "recovered_reported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    
    d_pred <- pred_clean %>%
      filter(section == "death_reported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    
    t_d <- r_pred + d_pred + p_pred
    total_pred <- rowSums(matrix(rowMeans(result$prediction), nrow = obs_days + t_pred)[, 3:9])
    UF_p <- total_pred / t_d
    
    d_u <- pred_clean %>%
      filter(section == "death_unreported") %>%
      dplyr::select(-(1:4)) %>%
      rowMeans()
    total_death <- d_u + d_pred
    UF_d <- total_death / d_pred
    ifr <- total_death / total_pred
    
    impo <- tibble(
      "underreporting_cases"  = UF_p[obs_days + 1],
      "underreporting_deaths" = UF_d[obs_days + 1],
      "ifr"                   = ifr[obs_days + 1]
    )
    
    fwrite(impo, paste0(wd, "/important_", state, "_", max_date, ".txt"))
    message("Successfully executed the call.")
  },
  error = function(e){
    message('Caught an error!')
    print(e)
  }
)
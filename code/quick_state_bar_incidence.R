source("libraries.R")
source("functions.R")

max_date <- "2021-03-22"

# get data ------------
state_data <- get_india_state_data(max_date = max_date)

# plot state barplots ----------
state_data %>%
  plot_daily_barplot(sub_state = "Delhi", max_date = max_date)
state_data %>%
  plot_daily_barplot(sub_state = "Maharashtra", max_date = max_date)
state_data %>%
  plot_daily_barplot(sub_state = "Kerala", max_date = max_date)

# plot india barplot ----------
plot_india_barplot(max_date = max_date)

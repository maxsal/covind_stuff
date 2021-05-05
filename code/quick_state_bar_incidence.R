source("libraries.R")
source("functions.R")

max_date <- "2021-05-03"

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

# for manuscript -------------
plot_india_barplot(start_date = "2020-03-15", max_date = "2021-03-15", save = TRUE)

# prepares data for bar and stack plots
prep_data <- function(pl) {
  d[place == pl & date >= (max_date - 365) & date <= max_date & daily_cases > 0 & daily_recovered > 0 & daily_deaths > 0][, .(date, daily_cases, daily_deaths, daily_recovered)]
}
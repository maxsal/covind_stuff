melt_data <- function(d) {
  melt(d, id.vars = "date", variable.name = "Trend", value.name = "count")[, Trend := data.table::fcase(
    Trend == "daily_cases", "New cases",
    Trend == "daily_deaths", "Fatalities",
    Trend == "daily_recovered", "Recovered"
  )]
}
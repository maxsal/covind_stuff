library(data.table)
library(janitor)
library(covid19india)

bharat_to_source_format <- function() {

  bharat <- fread("https://data.covid19bharat.org/csv/latest/state_wise_daily.csv",
                  showProgres = FALSE)

  bharat <- bharat[, !c("Date", "TT")]
  setnames(bharat, old = "Date_YMD", new = "date")

  bharat <- clean_names(bharat)

  bharat <- melt(bharat, id = 1:2)
  bharat <- dcast(bharat, date + variable ~ status)

  setnames(bharat, old = "variable", new = "abbrev")
  bharat <- merge.data.table(
    bharat,
    covid19india::pop[, .SD[1], by = place][, !c("population")],
    by = "abbrev",
    all.x = TRUE
  )

  bharat <- bharat[order(date), total_cases := cumsum(Confirmed), by = place][]
  bharat <- bharat[order(date), total_deaths := cumsum(Deceased), by = place][]
  bharat <- bharat[order(date), total_recovered := cumsum(Recovered), by = place][]

  bharat <- bharat[, !c("Confirmed", "Deceased", "Recovered", "abbrev")]
  bharat <- bharat[, .(State = place, Date = date, Cases = total_cases, Recovered = total_recovered, Deaths = total_deaths)][]

  return(bharat)

}

bharat_to_source_format()

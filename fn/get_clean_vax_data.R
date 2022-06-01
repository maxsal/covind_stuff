get_clean_vax_data <- function() {
  merge.data.table(
    get_state_vax(),
    covid19india::pop[, !c("population")][, .SD[1], by = "place"],
    by = "place", all.x = TRUE
  )[!is.na(total_doses)][date >= "2021-02-15"]
}
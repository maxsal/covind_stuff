library(tidyverse)
library(here)
library(glue)
library(janitor)
library(anytime)
options(stringsAsFactors = FALSE)

mh <- FALSE

d <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
              col_types = cols()) %>%
  clean_names()

scenarios <- c("2021-03-01", "2021-03-15", "2021-03-30",
               "2021-04-15", "2021-04-30", "no_intervention")

for (i in seq_along(scenarios)) {
  
  if (mh == TRUE) {
    tmp_filename <- glue("{scenarios[i]}_mh_smooth1_data.txt")
  } else {
    tmp_filename <- glue("{scenarios[i]}_smooth1_data.txt")
  }
  
  if (i == 1) {
    p <- read_tsv(here("lockdown", "data", "revised",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("lockdown", "data", "revised",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

dates <- as.Date(c(
  "2021-03-01",
  "2021-03-15",
  "2021-03-30",
  "2021-04-15",
  "2021-04-30",
  "2021-05-15",
  "2021-05-30",
  "2021-06-15"
))

obs <- d %>%
  select(date = date_ymd, value = total_confirmed) %>%
  filter(date %in% dates) %>%
  mutate(
    scenario = "Observed"
  )

proj <- p %>%
  filter(date %in% dates & scenario != "no intervention") %>%
  mutate(scenario = paste(trimws(format(as.Date(scenario), '%B')), trimws(format(as.Date(scenario), '%e')))) %>%
  filter(!(date == "2021-05-15" & scenario == "NA NA"))

table_cases <- bind_rows(obs, proj) %>%
  mutate(scenario = case_when(
    scenario == "NA NA" ~ "No intervention",
    T ~ scenario
  ))

table_cases <- table_cases %>%
  mutate(scenario = case_when(
    tolower(scenario) %in% c("observed", "no intervention") ~ "Observed/No Intervention",
    T ~ scenario
  )) %>%
  pivot_wider(
    names_from  = "scenario",
    values_from = "value",
    id_cols     = "date"
  )

table_cases_print <- table_cases %>%
  mutate_if(is.numeric, \(x) trimws(format(x, big.mark = ","))) %>%
  mutate_if(is.character, \(x) ifelse(x == "NA", "-", x))

table_deaths <- table_cases

Table.Deaths <- table_deaths
Projections  <- p %>%
  filter(scenario != "no intervention") %>%
  mutate(scenario = paste(trimws(format(as.Date(scenario), '%B')), trimws(format(as.Date(scenario), '%e')))) %>%
  filter(!(date == "2021-05-15" & scenario == "NA NA"))
Data         <- read.csv("https://api.covid19india.org/csv/latest/case_time_series.csv")

for (i in 1:nrow(Table.Deaths)) {
  if (Table.Deaths$date[i] %in% Data$Date_YMD) {
    Table.Deaths$`Observed/No Intervention`[i] = Data$Total.Deceased[Data$Date_YMD == Table.Deaths$date[i]]
  } else {
    Table.Deaths$`Observed/No Intervention`[i] = round(
      Table.Deaths$`Observed/No Intervention`[i] * (Data$Total.Deceased[Data$Date_YMD == "2021-04-30"] / Data$Total.Confirmed[Data$Date_YMD == "2021-04-30"])
    )
  }
}

for (i in 1:nrow(Table.Deaths)) {
  for (j in 3:7) {
    if (!is.na(Table.Deaths[i, j])) {
      Proj.Date = as.numeric(as.Date(Table.Deaths$date[i]))
      Lock.Date = as.numeric(as.Date(paste0(colnames(Table.Deaths)[j], ", 2021"), '%B %e, %Y'))
      Lock.Date.Text = gsub("(\\D)0", "\\1", format(as.Date(Lock.Date, origin = "1970-01-01"), "%B %d"))
      Difference = Proj.Date - Lock.Date
      
      Date.Series.Present = as.character(seq.Date(
        from = as.Date(Lock.Date, origin = "1970-01-01"),
        to = as.Date(Proj.Date, origin = "1970-01-01"),
        by = "day"
      ))
      
      Case.Series = Projections$incidence[Projections$date %in% as.Date(Date.Series.Present) &
                                            Projections$scenario == Lock.Date.Text]
      CFR.Series = Data$Daily.Deceased[Data$Date_YMD %in% Date.Series.Present] / Data$Daily.Confirmed[Data$Date_YMD %in% Date.Series.Present]
      CFR.Series = c(
        CFR.Series,
        rep(
          Data$Total.Deceased[Data$Date_YMD == Date.Series.Present[length(CFR.Series)]] / Data$Total.Confirmed[Data$Date_YMD == Date.Series.Present[length(CFR.Series)]],
          length(Case.Series) - length(CFR.Series)
        )
      )
      
      Deaths.After = sum(na.omit(Case.Series * CFR.Series))
      
      Deaths.Before = Projections$value[Projections$date == Date.Series.Present[1] &
                                          Projections$scenario == Lock.Date.Text] * (Data$Total.Deceased[Data$Date_YMD == Date.Series.Present[1]] / Data$Total.Confirmed[Data$Date_YMD == Date.Series.Present[1]])

      Table.Deaths[i, j] = round(Deaths.Before + Deaths.After)
    }
  }
}

Table.Deaths.Print <-  Table.Deaths

table_deaths_print <- Table.Deaths.Print %>%
  mutate_if(is.numeric, \(x) trimws(format(x, big.mark = ","))) %>%
  mutate_if(is.character, \(x) ifelse(x == "NA", "-", x))

if (mh == TRUE) {
  case_out  <- here("lockdown", "output", "table_mh_cases.txt")
  death_out <- here("lockdown", "output", "table_mh_deaths.txt")
} else {
  case_out  <- here("lockdown", "output", "table_cases.txt")
  death_out <- here("lockdown", "output", "table_deaths.txt")
}

write_tsv(table_cases_print,
          case_out)
write_tsv(table_deaths_print,
          death_out)

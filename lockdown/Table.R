library(tidyr)
library(anytime)
options(stringsAsFactors = FALSE)

Data = read.csv(file = url(
  "https://api.covid19india.org/csv/latest/case_time_series.csv"
))
Projections = read.delim(file = "Projections/june1_smooth_forecast.txt")
# CFRs = read.delim(file = "Projections/march25_cfr.txt")

Dates = c(
  "2021-03-01",
  "2021-03-15",
  "2021-03-30",
  "2021-04-15",
  "2021-04-30",
  "2021-05-15",
  "2021-05-30",
  "2021-06-15"
)

Observed = Data[as.character(Data$Date_YMD) %in% Dates, c(2, 4)]
colnames(Observed) = c("date", "value")
Observed$scenario = "Observed"

Projected = Projections[as.character(Projections$date) %in% Dates, c(1:2, 6)]
Projected = Projected[!(Projected$date == "2021-04-30" &
                          Projected$scenario == "No intervention"),]

Projected$scenario[Projected$scenario != "No intervention"] = as.character(anydate(paste0(Projected$scenario[Projected$scenario != "No intervention"], ", 2021")))

Table.Cases = rbind(Observed, Projected)
Table.Cases$scenario[Table.Cases$scenario %in% c("Observed", "No intervention")] = "Observed/No Intervention"
Table.Cases = spread(Table.Cases, scenario, value)
Table.Cases = Table.Cases[, c(1, 7, 2:6)]

Table.Cases.Print = Table.Cases

for (i in 1:nrow(Table.Cases))
{
  Table.Cases.Print[i, 2] = format(Table.Cases[i, 2],
                                   big.mark = ",",
                                   scientific = FALSE)
}

for (i in 1:nrow(Table.Cases))
{
  for (j in 3:7)
  {
    if (!is.na(Table.Cases[i, j]))
    {
      Table.Cases.Print[i, j] = paste0(
        format(
          Table.Cases[i, j],
          big.mark = ",",
          scientific = FALSE
        ),
        " (",
        format(
          as.numeric(Table.Cases[i, 2]) - as.numeric(Table.Cases[i, j]),
          big.mark = ",",
          scientific = FALSE
        ),
        ")"
      )
    }
    else
    {
      Table.Cases.Print[i, j] = "-"
    }
  }
}

Table.Deaths = Table.Cases

for (i in 1:nrow(Table.Deaths))
{
  if (Table.Deaths$date[i] %in% Data$Date_YMD)
  {
    Table.Deaths$`Observed/No Intervention`[i] = Data$Total.Deceased[Data$Date_YMD == Table.Deaths$date[i]]
  }
  else
  {
    Table.Deaths$`Observed/No Intervention`[i] = round(
      Table.Deaths$`Observed/No Intervention`[i] * (Data$Total.Deceased[Data$Date_YMD == "2021-04-30"] / Data$Total.Confirmed[Data$Date_YMD == "2021-04-30"])
    )
  }
}

for (i in 1:nrow(Table.Deaths))
{
  for (j in 3:7)
  {
    if (!is.na(Table.Deaths[i, j]))
    {
      Proj.Date = as.numeric(as.Date(Table.Deaths$date[i]))
      Lock.Date = as.numeric(as.Date(colnames(Table.Deaths)[j]))
      Lock.Date.Text = gsub("(\\D)0", "\\1", format(as.Date(Lock.Date, origin = "1970-01-01"), "%B %d"))
      Difference = Proj.Date - Lock.Date
      
      # Date.At = as.character(as.Date(as.numeric(as.Date("2020-03-25")) + Difference, origin = "1970-01-01"))
      # Date.At = as.character(as.Date(as.numeric(as.Date("2020-06-03")) + Difference, origin = "1970-01-01"))
      
      # Date.Series.Past = as.character(seq.Date(
      #   from = as.Date("2020-03-25"),
      #   to = as.Date(Date.At),
      #   by = "day"
      # ))
      
      # CFR.Series = Data$Daily.Deceased[Data$Date_YMD %in% Date.Series.Past] / Data$Daily.Confirmed[Data$Date_YMD %in% Date.Series.Past]
      # CFR.Series = CFRs$smooth_cfr[CFRs$date %in% Date.Series.Past]
      
      # if (length(CFR.Series) > 1)
      # {
      #   Index = 1:length(CFR.Series)
      #   Smoother = loess(CFR.Series ~ Index, span = 0.50)
      #   CFR.Series = predict(Smoother)
      # }
      
      Date.Series.Present = as.character(seq.Date(
        from = as.Date(Lock.Date, origin = "1970-01-01"),
        to = as.Date(Proj.Date, origin = "1970-01-01"),
        by = "day"
      ))
      
      Case.Series = Projections$incidence[Projections$date %in% Date.Series.Present &
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
      
      # CFR = Data$Total.Deceased[Data$Date_YMD == "2021-04-30"] / Data$Total.Confirmed[Data$Date_YMD == "2021-04-30"]
      # CFR = Data$Total.Deceased[Data$Date_YMD == Date.At] / Data$Total.Confirmed[Data$Date_YMD == Date.At]
      
      # Table.Deaths[i, j] = round(Table.Deaths[i, j] * CFR)
      
      Table.Deaths[i, j] = round(Deaths.Before + Deaths.After)
    }
  }
}

Table.Deaths.Print = Table.Deaths

for (i in 1:nrow(Table.Deaths))
{
  Table.Deaths.Print[i, 2] = format(Table.Deaths[i, 2],
                                    big.mark = ",",
                                    scientific = FALSE)
}

for (i in 1:nrow(Table.Deaths))
{
  for (j in 3:7)
  {
    if (!is.na(Table.Deaths[i, j]))
    {
      Table.Deaths.Print[i, j] = paste0(
        format(
          Table.Deaths[i, j],
          big.mark = ",",
          scientific = FALSE
        ),
        " (",
        format(
          as.numeric(Table.Deaths[i, 2]) - as.numeric(Table.Deaths[i, j]),
          big.mark = ",",
          scientific = FALSE
        ),
        ")"
      )
    }
    else
    {
      Table.Deaths.Print[i, j] = "-"
    }
  }
}

write.csv(x = Table.Cases.Print,
          file = "Table_Cases.csv",
          row.names = FALSE)

write.csv(x = Table.Deaths.Print,
          file = "Table_Deaths.csv",
          row.names = FALSE)
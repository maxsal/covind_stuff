# pull data from JHU CSSE COVID-19 database -------------
get_jhu_data <- function(path = NULL) {
  
  if (is.null(path)) {
    message("using default path")
    path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  }
  
  readr::read_csv(path, col_types = cols()) %>%
  dplyr::rename(
    country = `Country/Region`
  )
  
}

# quickly tidy JHU CSSE COVID-19 data for a country of interest -----------
tidy_country <- function(data, place) {
  data %>% 
    filter(country == place & is.na(`Province/State`)) %>%
    select(-c(`Province/State`, Lat, Long)) %>%
    pivot_longer(
      names_to  = "date",
      values_to = "count",
      -country
    ) %>%
    mutate(date = as.Date(date, "%m/%d/%y")) %>%
    filter(count != 0) %>%
    arrange(date) %>%
    mutate(
      incidence = count - dplyr::lag(count)
    )
}

# pull and clean national level data from covid19india.org ----------
get_covid19india_data <- function(path = NULL, death = FALSE) {
  
  if (is.null(path)) {
    path <- "https://api.covid19india.org/csv/latest/case_time_series.csv"
  }
  
  if (death == FALSE) {
    readr::read_csv(path, col_types = cols()) %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      country = "India"
    ) %>%
    dplyr::select(country, date = date_ymd, count = total_confirmed) %>%
    tidyr::drop_na(date, count) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      incidence = count - dplyr::lag(count)
    )
  }
  
  if (death == TRUE) {
    readr::read_csv(path, col_types = cols()) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        country = "India"
      ) %>%
      dplyr::select(country, date = date_ymd, count = total_deceased) %>%
      tidyr::drop_na(date, count) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        incidence = count - dplyr::lag(count)
      )
  }
  
}

# covind ggplot theme -----------
covind_theme <- function(font_fam = "Lato", legend_pos = "top") {
  theme_minimal() +
  theme(
    text            = element_text(family = font_fam),
    plot.title      = ggtext::element_markdown(size = 18, face = "bold"),
    plot.subtitle   = element_text(size = 14, color = "#36454f"),
    axis.text       = element_text(size = 10, color = "#36454f"),
    axis.title      = element_text(size = 12, face = "italic"),
    legend.position = legend_pos,
    legend.title    = element_blank(),
    plot.caption    = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
    axis.line       = element_line(color="black", size = 0.5),
    panel.grid      = element_blank()
  )
}

# COVID-19 incidence by country plot -----------
plot_cov_inc_by_country <- function(data, countries,
                                    title = "COVID-19 incidence by country",
                                    y_axis = "Daily cases") {
  
  if ("India" %in% countries) {
    cap <- "**Source:** covid19india.org (India); JHU CSSE GitHub (non-India)"
  } else {
    cap <- "**Source:** JHU CSSE GitHub"
  }
  
  data %>%
    dplyr::filter(country %in% countries) %>%
    ggplot(aes(x = date, y = incidence, group = country, color = country)) +
    geom_smooth(method = "loess", formula = "y ~ x", span = 0.15, se = FALSE, size = 1) +
    labs(
      title    = title,
      subtitle = glue::glue("from {format(min(data$date), '%B %e %Y')} to {format(max(data$date), '%B %e %Y')}"),
      x        = "Date",
      y        = y_axis,
      caption  = cap
    ) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Dark2") +
    covind_theme()
  
}

# get india state list ------------
get_india_state_list <- function() {
  
  states_map <- c("Andhra Pradesh" =  "AP", "Arunachal Pradesh" =  "AR",
                  "Assam" =  "AS", "Bihar" =  "BR", "Chhattisgarh" =  "CG", "Goa" =  "GA",
                  "Gujarat" =  "GJ", "Haryana" =  "HR", "Himachal Pradesh" =  "HP",
                  "Jammu and Kashmir" =  "JK", "Jharkhand" =  "JH", "Karnataka" =  "KA",
                  "Kerala" =  "KL", "Madhya Pradesh" =  "MP",  "Maharashtra" =  "MH",
                  "Manipur" =  "MN", "Meghalaya" =  "ML", "Mizoram" =  "MZ", "Nagaland" =  "NL",
                  "Odisha" =  "OR", "Punjab" =  "PB", "Rajasthan" =  "RJ", "Sikkim" =  "SK",
                  "Tamil Nadu" =  "TN", "Tripura" =  "TR", "Uttarakhand" =  "UK",
                  "Uttar Pradesh" =  "UP", "West Bengal" =  "WB", "Tamil Nadu" =  "TN",
                  "Tripura" =  "TR", "Andaman and Nicobar Islands" =  "AN",
                  "Chandigarh" =  "CH", "Dadra and Nagar Haveli" =  "DH",
                  "Daman and Diu" = "DD", "Delhi" =  "DL", "Lakshadweep" =  "LD",
                  "Pondicherry" =  "PY", "Telangana" =  "TG", "Dadra and Nagar Haveli" =  "DN",
                  "Chhattisgarh" =  "CT", "Ladakh" =  "LA", "Uttarakhand" =  "UT"
  )
  
  x <- names(states_map)
  names(x) <- states_map
  
  return(x)
}

# get india state-level COVID-19 data ------------
get_india_state_data <- function(state_list = get_india_state_list(),
                                 max_date) {
  
  request <- GET("https://api.covid19india.org/states_daily.json")
  json    <- content(request)
  data    <- map_dfr(json[[1]], ~ .x)
  
  tmp <- read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                  col_types = cols()) %>%
    clean_names() %>%
    select(-date) %>%
    rename(date = date_ymd)
  
  tmp$tt <- NULL
  
  data$tt <- NULL
  state_codes <- setdiff(names(tmp), c("date", "status", "dateymd"))
  
  tib <- tmp %>%
    tidyr::pivot_longer(
      names_to = "state",
      values_to = "count",
      cols = !!state_codes
    ) %>%
    dplyr::mutate(
      count = as.numeric(count)
    ) %>%
    tidyr::pivot_wider(
      names_from = "status",
      values_from = "count"
    ) %>%
    tidyr::unnest(cols = c(Confirmed, Recovered, Deceased)) %>%
    dplyr::rename(
      `Daily Cases`     = Confirmed,
      `Daily Deaths`    = Deceased,
      `Daily Recovered` = Recovered, 
      Date              = date,
      State             = state
    ) %>%
    dplyr::mutate(
      Name = recode(str_to_upper(State), !!!state_list)
    ) %>%
    dplyr::arrange(State, Date) %>%
    dplyr::group_by(State) %>%
    dplyr::mutate(
      Cases = accumulate(`Daily Cases`, `+`),
      Deaths = accumulate(`Daily Deaths`, `+`),
      Recovered = accumulate(`Daily Recovered`, `+`)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Date >= "2020-03-15" & Date <= max_date) %>%
    janitor::clean_names()
  
  return(tib)
  
}

# colors for daily bar plot ------------
daily_barplot_colors <- c(
  "Fatalities" = "#ED553B",
  "New cases"  = "#f2c82e",
  "Cases"  = "#f2c82e",
  "Recovered"  = "#138808"
)

# daily bar plot -----------
plot_daily_barplot <- function(data, sub_state, start_date = "2020-03-01",
                               max_date, save = TRUE) {
  
  tmp_plt <- data %>%
    filter(date >= start_date & date <= max_date & name == sub_state) %>%
    filter(daily_cases > 0 & daily_recovered > 0 & daily_deaths > 0) %>%
    select(-c(cases, deaths, recovered, state, name)) %>%
    pivot_longer(names_to = "Trend", values_to = "count", -c(date)) %>%
    mutate(
      Trend = as.factor(case_when(
        Trend == "daily_cases" ~ "New cases",
        Trend == "daily_deaths" ~ "Fatalities",
        Trend == "daily_recovered" ~ "Recovered"
      ))
    ) %>%
    ggplot(aes(x = date, y = count, group = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")), fill = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = daily_barplot_colors) +
    scale_y_continuous(labels = scales::comma) +
    guides(fill = guide_legend(title = "", override.aes = list(size = 1))) +
    labs(
      title    = glue("Daily number of COVID-19 cases, fatalities, and recovered in {sub_state}"),
      subtitle = glue("as of {format(as.Date(max_date), '%B %e')}"),
      x        = "Date",
      y        = "Count",
      caption  = "**\uA9 COV-IND-19 Study Group**<br>**Source:** covid19data.org"
    ) +
    covind_theme() +
    theme(legend.position = "top",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.3, "cm"))
  
  if (save == TRUE) {
    message("plot saved to fig folder")
    ggsave(plot     = tmp_plt,
           filename = here("fig", glue("{sub_state}_barplot_{max_date}.pdf")),
           width    = 12, height = 6,
           device   = cairo_pdf)
    
  }
  
  return(tmp_plt)
  
}

# plot india bar plot ----------
plot_india_barplot <- function(start_date = "2020-03-01", max_date, save = TRUE) {
  
  message("pulling latest data from covid19india.org")
  
  tmp <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                   col_types = cols()) %>%
    clean_names()
  
  tmp_plt <- tmp %>%
    dplyr::filter(date_ymd >= start_date & date_ymd <= max_date) %>%
    dplyr::select(-c(total_confirmed, total_deceased, total_recovered, date)) %>%
    tidyr::pivot_longer(names_to = "Trend", values_to = "count", -c(date_ymd)) %>%
    dplyr::mutate(
      Trend = as.factor(case_when(
        Trend == "daily_confirmed" ~ "New cases",
        Trend == "daily_deceased" ~ "Fatalities",
        Trend == "daily_recovered" ~ "Recovered"
      ))
    ) %>%
    ggplot(aes(x = date_ymd, y = count, group = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")), fill = factor(Trend, levels = c("Recovered", "Fatalities", "New cases")))) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = daily_barplot_colors) +
    scale_y_continuous(labels = scales::comma) +
    guides(fill = guide_legend(title = "", override.aes = list(size = 1))) +
    labs(
      title    = glue("Daily number of COVID-19 cases, fatalities, and recovered in India"),
      subtitle = glue("as of {format(as.Date(max_date), '%B %e')}"),
      x        = "Date",
      y        = "Count",
      caption  = "**\uA9 COV-IND-19 Study Group**<br>**Source:** covid19data.org"
    ) +
    covind_theme() +
    theme(legend.position = "top",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.3, "cm"),
          plot.margin = margin(10, 20, 10, 10))
  
  if (save == TRUE) {
  
    message("plot saved to fig folder")
    ggsave(plot     = tmp_plt,
           filename = here("fig", glue("india_barplot_{max_date}.pdf")),
           width    = 12, height = 6,
           device   = cairo_pdf)
    
  }
  
  return(tmp_plt)
  
}

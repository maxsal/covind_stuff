# get count data ----------
get_count_data <- function(top_n = FALSE, abbrevs = NULL) {
    
    message("getting count data...")
    
    # population data
    pops <- readr::read_csv("https://raw.githubusercontent.com/umich-cphds/cov-ind-19/master/model/populations.csv", col_types = cols()) %>%
        dplyr::rename(place = full)
    
    # national-level count data
    nat_dat <- readr::read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv", col_types = cols()) %>%
        janitor::clean_names() %>%
        dplyr::select(
            date         = date_ymd,
            daily_cases  = daily_confirmed,
            daily_deaths = daily_deceased,
            daily_recovered,
            cases     = total_confirmed,
            deaths    = total_deceased,
            recovered = total_recovered) %>%
        # dplyr::filter(country == "India") %>%
        dplyr::mutate(place = "India") %>%
        # dplyr::arrange(date) %>%
        # dplyr::mutate(
        #     daily_cases     = cases - dplyr::lag(cases),
        #     daily_deaths    = deaths - dplyr::lag(deaths),
        #     daily_recovered = recovered - dplyr::lag(recovered)
        # ) %>%
        tibble::add_column(abbrev = "India")
    
    # state-level count data
    state_dat <- readr::read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv", col_types = cols()) %>%
        janitor::clean_names() %>%
        dplyr::select(-c(date, tt, un)) %>%
        dplyr::rename(date = date_ymd) %>%
        tidyr::pivot_longer(
            names_to = "abbrev",
            values_to = "value",
            cols = -c("date", "status")
        ) %>%
        tidyr::pivot_wider(
            names_from = "status",
            values_from = "value",
            id_cols = c("date", "abbrev")
        ) %>%
        dplyr::rename(
            daily_cases     = Confirmed,
            daily_recovered = Recovered,
            daily_deaths    = Deceased
        ) %>%
        dplyr::group_by(abbrev) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            cases     = cumsum(daily_cases),
            recovered = cumsum(daily_recovered),
            deaths    = cumsum(daily_deaths)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(pops %>% select(-population), by = "abbrev")
    
    if (top_n == TRUE) {
        if (is.null(abbrevs)) {
            abbrevs <- state_dat %>%
                dplyr::group_by(place) %>%
                dplyr::filter(cases == max(cases)) %>%
                dplyr::ungroup() %>%
                dplyr::arrange(desc(cases)) %>%
                utils::head(20) %>%
                filter(abbrev != "dd") %>%
                dplyr::pull(place)
        }
        
        state_dat <- state_dat %>% dplyr::filter(place %in% abbrevs)
        
    }
    
    dplyr::bind_rows(nat_dat, state_dat)
    
}

# get testing data ----------
get_testing_data <- function() {
    
    message("getting testing data...")
    
    # population data
    pops <- readr::read_csv("https://raw.githubusercontent.com/umich-cphds/cov-ind-19/master/model/populations.csv", col_types = cols()) %>%
        dplyr::rename(place = full)

    # national-level testing data
    request  <- httr::GET("https://api.covid19india.org/data.json")
    json     <- content(request)
    data     <- map_dfr(json[[1]], ~ .x)
    nat_test <- map_dfr(json[['tested']], ~ .x) %>%
        select(
            Cases = totalpositivecases,
            Tests = totalsamplestested,
            Date  = testedasof
        ) %>%
        mutate(
            Date    = as.Date(word(Date, 1), format = "%d/%m/%Y"),
            Cases   = as.numeric(str_remove(Cases, ",")),
            Tests   = as.numeric(str_remove(Tests, ",")),
            Country = "India"
        ) %>%
        janitor::clean_names() %>%
        dplyr::select(date, place = country, total_tests = tests) %>%
        dplyr::group_by(date) %>%
        dplyr::filter(total_tests == max(total_tests)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            daily_tests = total_tests - dplyr::lag(total_tests),
            ppt         = total_tests / (pops %>% dplyr::filter(place == "India") %>% dplyr::pull(population))
        )
    
    # state-level testing data
    state_test <- suppressWarnings(readr::read_csv("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv",
                                  col_types = cols())) %>%
        janitor::clean_names() %>%
        dplyr::mutate(
            date              = as.Date(updated_on, "%d/%m/%Y"),
            tests_per_million = total_tested / 1332830000 * 1e6,
            ppt               = tests_per_million / 1e6
        ) %>%
        dplyr::select(date, place = state, total_tests = total_tested, ppt) %>%
        dplyr::left_join(pops, by = "place") %>%
        dplyr::group_by(place) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(
            daily_tests = total_tests - dplyr::lag(total_tests),
            ppt         = total_tests / population) %>%
        dplyr::ungroup() %>%
        dplyr::select(-population)
    
    # combine into single table
    dplyr::bind_rows(nat_test, state_test)
    
}

merge_data <- function(count, test, tpr_d = 0.02) {
    
    message("merging data...")
    
    dplyr::left_join(
        count,
        test %>% select(-abbrev),
        by = c("place", "date")
    ) %>%
        dplyr::mutate(
            tpr       = cases / total_tests,
            shortfall = ((tpr / tpr_d) - 1) * total_tests
        ) %>%
        dplyr::mutate(
            shortfall = case_when(
                shortfall < 0 ~ 0,
                TRUE ~ shortfall
            )
        )
    
}

# do it all ----------
do_it_all <- function() {
    
    count_dat <- get_count_data()
    test_dat  <- get_testing_data()
    merge_dat <- merge_data(count = count_dat, test = test_dat) 
    # %>%
    #     get_dbl()
    r0_dat <- get_r0(merge_dat) %>%
        dplyr::select(
            place,
            date,
            r_est = r,
            r_lower = lower,
            r_upper = upper
        )
    
    merge_dat %>%
        left_join(r0_dat, by = c("place", "date"))
    
    
}

# forecast data
get_abbrevs <- function(d) {
    
    d %>% pull(abbrev) %>% unique()
    
}

get_forecast_data <- function(d, ab) {
    
    for (i in seq_along(abbrevs)) {
        
        super_tmp_dat <- readr::read_tsv(glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{d}/1wk/{ab[i]}_plot_data.txt"),
                                         col_types = cols()) %>%
            add_column(abbrev = ab[i])
        
        if (i == 1) {
            
            tmp <- super_tmp_dat
            
        } else {
            tmp <- bind_rows(tmp, super_tmp_dat)
        }
        
    }
    
    return(tmp)
    
}

# ggplot theme ----------
covind19_base <- theme_minimal() +
    theme(
        # text               = element_text(family = "Helvetica Neue"),
        plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
        axis.text          = element_text(size = 10, color = "#36454f"),
        axis.title         = element_text(size = 12, face = "italic"),
        legend.position    = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
    )

# cfr calculation function ------------
CFR <- function(C,D) {
    cfr       <- D / C
    cfr_logit <- log(cfr) - log(1 - cfr)
    sd_logit  <- sqrt(C / (D * (C - D)))
    
    lower_logit <- cfr_logit - qnorm(0.975) * sd_logit
    upper_logit <- cfr_logit + qnorm(0.975) * sd_logit
    
    upper <- exp(upper_logit) / (1 + exp(upper_logit))
    lower <- exp(lower_logit) / (1 + exp(lower_logit))
    
    return(c(cfr, upper, lower))
}

# calculate doubling time ----------
dbl_timr <- function(data, end_date, time = 7) {
    
    start <-  data %>% 
        dplyr::filter(date == as.Date(as.Date(end_date) - time)) %>% 
        dplyr::pull(cases)
    
    if (length(start) == 0) {
        NA
    } else if (start == 0) {
        NA
    } else {
        end   <- data %>%
            dplyr::filter(date == as.Date(end_date)) %>%
            dplyr::pull(cases)
        
        r <- ((end - start) / start) * 100
        
        time * (log(2) / log(1 + (r / 100)))
    }
}

# est_r for states function -----------
estR0_out <- function(dat) {
    
    # if (dim(dat)[1] < 7) {
    #   NA
    # } else {
    
    t_start <- seq(2, nrow(dat) - 4)
    t_end   <- t_start + 4
    
    res <- EpiEstim::estimate_R(
        incid = dat$daily_cases,
        method = "parametric_si",
        config = make_config(list(
            mean_si             = 7,
            std_si              = 4.5,
            si_parametric_distr = "G",
            t_start             = t_start,
            t_end               = t_end,
            seed                = set_seed))
    ) 
    
    tibble::tibble(
        date_num = res$dates
    ) %>% dplyr::left_join(
        res$R, by = c("date_num" = "t_end")
    ) %>%
        dplyr::select(
            date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
        ) %>%
        tibble::add_column(date = dat$date) %>%
        dplyr::select(-date_num) %>%
        dplyr::select(date, tidyselect::everything())
    
    # }
}


# get cfr ----------
get_cfr <- function(dat) {
    
    tmp <- dat %>%
        dplyr::group_by(place) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::ungroup()
    
    tmp <- tmp %>%
        dplyr::select(
            place = place,
            C     = cases,
            D     = deaths
        )
    
    tmp_out <- tibble(
        place  = tmp$place,
        cfr   = rep(0,nrow(tmp)),
        upper = rep(0,nrow(tmp)),
        lower = rep(0,nrow(tmp))
    )
    
    for (i in 1:nrow(tmp_out)) {
        C <- tmp$C[i]
        D <- tmp$D[i]
        
        result <- CFR(C,D)
        
        tmp_out$cfr[i]   <- result[1]
        tmp_out$upper[i] <- result[2]
        tmp_out$lower[i] <- result[3]
        
    }
    
    tmp_out %>%
        dplyr::mutate(
            place = case_when(
                place == "India" ~ "National estimate",
                TRUE ~ place
            )
        )
    
}

# get_dbl -----------

get_dbl <- function(dat) {
    
    message("getting doubling time...")
    
    places <- unique(dat$place)
    
    for (i in seq_along(places)) {
        
        tmp_dat <- dat %>%
            dplyr::filter(place == places[i]) %>%
            tibble::add_column(dbl = NA) %>%
            dplyr::arrange(date)
        
        for (j in seq_along(tmp_dat$date)) {
            
            tmp_dat$dbl[j] <- dbl_timr(data = tmp_dat, end_date = tmp_dat$date[j])
            
        }
        
        tmp_dat$dbl[is.infinite(tmp_dat$dbl)] <- NA
        
        txt <- glue("tmp_{unique(tmp_dat$abbrev)} <- tmp_dat")
        eval(parse(text = txt))
        
    }
    
    eval(parse(text = glue("bind_rows({paste(paste0('tmp_', unique(dat$abbrev)), collapse = ', ')})")))
    
}

# get_r0 -----------
get_r0 <- function(dat) {
    
    message("getting r0...")
    
    tmp_dat <- dat %>%
        dplyr::filter(daily_cases > 0 & cases >= 50) %>%
        dplyr::group_by(place) %>%
        dplyr::mutate(
            ns = n()
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(ns >=7)
    
    options(warn = -1)
    tmp_est <- tmp_dat %>%
        dplyr::select(date, daily_cases, place) %>%
        tidyr::nest(data = c(-place)) %>%
        dplyr::mutate(
            estR0 = purrr::map(data, ~estR0_out(dat = .x))
        ) %>%
        tidyr::unnest(estR0) %>%
        dplyr::select(-data) %>%
        dplyr::filter(date >= "2020-03-23") %>%
        tidyr::drop_na()
    options(warn = 1)
    
    return(tmp_est)
}

# get_r_est ----------
get_r_est <- function(dat) {
    
    dat %>%
        dplyr::group_by(place) %>%
        dplyr::slice((n()-6):n()) %>%
        dplyr::summarize(
            r       = mean(r_est, na.rm = TRUE),
            lower   = mean(r_lower, na.rm = TRUE),
            upper   = mean(r_upper, na.rm = TRUE),
            .groups = "drop_last"
        ) %>%
        dplyr::ungroup()
    
}

# get_tpr_est -----------
get_tpr_est <- function(dat) {
    
    dat %>%
        group_by(place) %>%
        slice((n()-6):n()) %>%
        summarize(
            tpr_min = min(tpr, na.rm = T),
            tpr_max = max(tpr, na.rm = T),
            tpr     = mean(tpr, na.rm = T),
            .groups = "drop_last"
        ) %>%
        drop_na(tpr) %>%
        ungroup()
    
}

### get vaccine data ----------
vax <- read_csv("https://api.covid19india.org/csv/latest/cowin_vaccine_data_statewise.csv",
                col_types = cols()) %>%
    clean_names()

# extract latest values for metrics table -----------
extract_latest <- function(data, group = place, cols = c("total_tests", "tpr", "dbl", "ppt")) {
  out <- data %>%
    group_by({{ group }}) %>%
    filter(date == max(date)) %>%
    distinct(date, .keep_all = TRUE) %>%
    ungroup() %>%
    select({{ group }}, date, all_of(cols))
  if ("India" %in% data[[paste0(substitute(group))]]) {
    out[[paste0(substitute(group))]] <- recode(out[[paste0(substitute(group))]],
                                               "India" = "National estimate")
  }
  return(out)
}

### make metrics tables ----------
get_metrics_tables <- function() {
    
    message("prepping...")
  
    today           <- Sys.Date() - 1
    tp              <- dat
    cfr1            <- cfr
    r_est           <- r0_est
    india_state_pop <- readr::read_csv("https://raw.githubusercontent.com/umich-cphds/cov-ind-19/master/model/populations.csv", col_types = cols()) %>%
        dplyr::rename(place = full)
    
    # pull abbrevs -----------
    use_abbrevs <- tp %>% filter(abbrev != "la") %>% pull(abbrev) %>% unique() %>% tolower()
    
    # state data ----------
    vax_dat <- readr::read_csv("http://api.covid19india.org/csv/latest/cowin_vaccine_data_statewise.csv",
                                col_types = cols()) %>%
      janitor::clean_names() %>%
      dplyr::mutate(updated_on = as.Date(updated_on, format = "%d/%m/%Y")) %>%
      dplyr::select(date = updated_on, state,
                    second_dose = second_dose_administered,
                    total_vax = total_individuals_vaccinated,
                    total_vax_doses = total_doses_administered) %>%
      dplyr::mutate(daily_vax_dose = total_vax_doses - dplyr::lag(total_vax_doses)) %>%
      tidyr::drop_na() %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::left_join(india_state_pop, c("state" = "place")) %>% 
      dplyr::mutate(
        pct_at_least_one = round((total_vax/population)*100, 2),
        pct_second = round((second_dose/population)*100, 2)
      ) %>% 
      dplyr::select(-population) %>%
      dplyr::mutate(state = ifelse(state == "India", "National estimate", state))
    
    sf <- tp %>%
      dplyr::group_by(place) %>%
      dplyr::filter(date > max(as.Date(date)) - 7) %>%
      dplyr::mutate(
        dailyTPR7 = daily_cases/daily_tests,
        dailyCFR7 = daily_deaths/daily_cases
      ) %>%
      dplyr::filter(is.finite(dailyTPR7)) %>%
      dplyr::mutate(dailyTPR7d = mean(dailyTPR7, na.rm = T),
                    dailyCFR7d = mean(dailyCFR7, na.rm = T)) %>%
      dplyr::filter(date == max(as.Date(date))) %>%
      distinct(date, .keep_all = TRUE) %>%
      ungroup() %>%
      dplyr::select(place, total_tests, ppt, shortfall, dailyTPR7d, dailyCFR7d,
                    daily_cases, daily_deaths, daily_tests, cases, deaths) %>%
      mutate(
        place = case_when(
          place == "India" ~ "National estimate",
          TRUE ~ place
        ),
        shortfall    = trimws(format(round(shortfall), big.mark = ",")),
        total_tested = trimws(format(total_tests, big.mark = ",")),
        ppt          = round(ppt * 100, digits = 2) 
      ) %>%
      left_join(india_state_pop, by = c("place")) %>%
      left_join(vax_dat, by = c("place" = "state"))
    
    # table ----------
    tib <- cfr1 %>%
      dplyr::distinct(place, .keep_all = TRUE) %>%
      dplyr::left_join(r_est %>%
                         mutate(place = recode(place, "India" = "National estimate")),
                       by = c("place")) %>%
      dplyr::left_join(tp %>%
                         extract_latest(cols = c("tpr")),
                       by = c("place")) %>%
      dplyr::left_join(sf, by = c("place")) %>%
      dplyr::mutate(perc_vaccine   = pct_at_least_one,
                    total_vacc     = format(total_vax, big.mark = ","),
                    daily_cases    = format(daily_cases, big.mark = ","),
                    daily_deaths   = format(daily_deaths, big.mark = ","),
                    daily_tests    = format(daily_tests, big.mark = ","),
                    daily_vax_dose = format(daily_vax_dose, big.mark = ","),
                    cases          = format(cases, big.mark = ","),
                    deaths         = format(deaths, big.mark = ",")) %>% 
      dplyr::rename(
        `# daily new cases`              = daily_cases,
        `# daily new deaths`             = daily_deaths,
        `7-day average daily TPR`        = dailyTPR7d,
        `7-day average daily CFR`        = dailyCFR7d,
        R                                = r,
        `daily tests`                    = daily_tests,
        `daily vaccine doses`            = daily_vax_dose,
        Location                         = place,
        CFR                              = cfr,
        `total cases`                    = cases,
        `total deaths`                   = deaths,
        `TPR`                            = tpr,
        `Total tested`                   = total_tested,
        `Testing shortfall`              = shortfall,
        `Percent with at least one dose` = perc_vaccine,
        `Total doses`                    = total_vacc,
        `% pop. with two shots`          = pct_second,
        `% pop. with at least one shot`  = pct_at_least_one
      ) %>%
      arrange(desc(`total cases`)) %>%
      mutate(
        `Testing shortfall`       = trimws(`Testing shortfall`),
        `7-day average daily CFR` = round(`7-day average daily CFR`, digits = 3)
      ) %>%
      dplyr::select(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                    `7-day average daily CFR`, Location, R, `daily tests`,
                    `daily vaccine doses`, CFR, `Total tested`, `total cases`,
                    `total deaths`, `Total doses`, `TPR`,`% pop. with two shots`,
                    `% pop. with at least one shot`)
    
    tib <- tib %>%
        select(-`Total tested`) %>% 
        mutate(Location = case_when(
            Location == "National estimate" ~ "India",
            TRUE ~ Location)
        ) %>%
        distinct() %>%
        mutate_if(is.character, trimws) %>%
        drop_na(`# daily new cases`) %>%
        dplyr::filter(`# daily new cases` != "NA")
    
    message("making full table...")
    
    source_note_text <- glue(
      "**\uA9 COV-IND-19 Study Group**<br>**Source data:** covid19india.org<br>
      **Notes:** Cells highlighted in green indicates good performance for given metric while red indicates need for improvement.
      Only states/union territories with the highest cumulative case counts as of {format(today, '%B %e')} are shown. 
      States are omitted if they have missing case count data.
      <br>
      **Abbrev:** CFR, Case-fatality rate."
    )
    
    tabl <- tib %>%
        gt() %>%
        # format table body text
        tab_style(
            style     = cell_text(size = px(14), font = "helvetica"),
            locations = cells_body()
        ) %>%
        tab_style(
            style     = cell_text(weight = "bold"),
            locations = cells_body((Location))
        ) %>%
        # format column names
        tab_style(
            style = cell_text(
                size      = px(12),
                color     = "#999",
                font      = "helvetica",
                transform = "uppercase"
            ),
            locations = cells_column_labels(everything())
        ) %>%
        # format numbers
        fmt_number(
            columns  = c(CFR, `7-day average daily TPR`, TPR),
            decimals = 3
        ) %>%
        fmt_number(
            columns  = c(R),
            decimals = 2
        ) %>%
        # random formatting
        tab_options(
            column_labels.border.top.style    = "none",
            column_labels.border.bottom.width = 1,
            column_labels.border.bottom.color = "#334422",
            table_body.border.bottom.color    = "#0000001A",
            data_row.padding                  = px(4)
        ) %>%
        # column widths
        cols_width(
            Location ~ px(150),
            c(R, CFR) ~ px(75),
            everything() ~ px(100)
        ) %>%
        cols_align(
            align   = "center",
            columns = everything()
        ) %>%
        # title
        tab_header(
            title    = md("**Assessing COVID-19 in India**"),
            subtitle = glue("as of {format(today, '%B %e')}")
        ) %>%
        # caption
        tab_source_note(
            source_note = md(source_note_text)
        ) %>% 
        # add and format column spanners
        tab_spanner(
            label   = "Point in time metrics",
            columns = c(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                        `7-day average daily CFR`, R, `daily tests`, `daily vaccine doses`)
        ) %>%
        tab_spanner(
            label   = "Cumulative metrics",
            columns = c(`total cases`, `total deaths`, `TPR`, CFR, 
                        `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
        ) %>% 
        cols_move_to_start((Location)) %>%
        tab_style(
            style = cell_text(
                size      = px(14),
                color     = "#999",
                font      = "helvetica",
                transform = "uppercase"
            ),
            locations = cells_column_spanners(spanners = c("Point in time metrics", "Cumulative metrics"))
        ) %>%
        # adjust title font
        tab_style(
            style     = list(cell_text(font = "helvetica", size = px(24))),
            locations = list(cells_title(groups = "title"))
        ) %>%
        # adjust subtitle font
        tab_style(
            style     = list(cell_text(font = "helvetica", size = px(18))),
            locations = list(cells_title(groups = "subtitle"))
        ) %>%
        # color cells based on values
        data_color(
            columns = c(R),
            colors = col_bin(c( "#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1.5,1000), pretty = F)
        ) %>%
        data_color(
            columns = c(`7-day average daily TPR`),
            colors = col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.05, 1), pretty = F, na.color = "#e8e8e8")
        ) %>%
        # highlight national estimate
        tab_style(
            style = cell_fill(color = "#fcf8d4"),
            locations = cells_body(
                rows = Location == "India")
        ) %>% 
        tab_style(
            style = cell_borders(sides = "left"),
            locations = cells_body(columns = (`total cases`))
        ) %>% 
        tab_style(
            style = cell_borders(sides = "left"),
            locations = cells_column_labels(columns = (`total cases`))
        ) %>% 
        tab_style(
            style = cell_borders(sides = "left"),
            locations = cells_column_spanners(("Cumulative metrics"))
        )
    
    message("making point-in-time table...")
    # new table
    point_in_time <- tib %>%
        select(-`total cases`, -`total deaths`, -`TPR`, -CFR, 
               -`Total doses`, -`% pop. with two shots`, -`% pop. with at least one shot`) %>%
        gt() %>%
        # format table body text
        tab_style(
            style     = cell_text(size = px(14), font = "helvetica"),
            locations = cells_body()
        ) %>%
        tab_style(
            style     = cell_text(weight = "bold"),
            locations = cells_body(c(Location))
        ) %>%
        # format column names
        tab_style(
            style = cell_text(
                size      = px(12),
                color     = "#999",
                font      = "helvetica",
                transform = "uppercase"
            ),
            locations = cells_column_labels(everything())
        ) %>%
        # format numbers
        fmt_number(
            columns  = c(`7-day average daily TPR`),
            decimals = 3
        ) %>%
        fmt_number(
            columns  = c(R),
            decimals = 2
        ) %>%
        # random formatting
        tab_options(
            column_labels.border.top.style    = "none",
            column_labels.border.bottom.width = 1,
            column_labels.border.bottom.color = "#334422",
            table_body.border.bottom.color    = "#0000001A",
            data_row.padding                  = px(4)
        ) %>%
        # column widths
        cols_width(
            Location ~ px(150),
            R ~ px(75),
            everything() ~ px(100)
        ) %>%
        cols_align(
            align   = "center",
            columns = everything()
        ) %>%
        # title
        tab_header(
            title    = md("**Assessing COVID-19 in India**"),
            subtitle = glue("as of {format(today, '%B %e')}")
        ) %>%
        # caption
        tab_source_note(
            source_note = md(source_note_text)
        ) %>% 
        tab_spanner(
            label   = "Point in time metrics",
            columns = c(`# daily new cases`, `# daily new deaths`, `7-day average daily TPR`,
                        `7-day average daily CFR`, R, `daily tests`, `daily vaccine doses`)
        ) %>% 
        cols_move_to_start((Location)) %>%
        tab_style(
            style = cell_text(
                size      = px(14),
                color     = "#999",
                font      = "helvetica",
                transform = "uppercase"
            ),
            locations = cells_column_spanners(spanners = c("Point in time metrics")) #, glue("Predictions on ({format(today + 21, '%m/%d')}) (No intervention)")
        ) %>%
        # adjust title font
        tab_style(
            style     = list(cell_text(font = "helvetica", size = px(24))),
            locations = list(cells_title(groups = "title"))
        ) %>%
        # adjust subtitle font
        tab_style(
            style     = list(cell_text(font = "helvetica", size = px(18))),
            locations = list(cells_title(groups = "subtitle"))
        ) %>%
        # color cells based on values
        data_color(
            columns = c(R),
            colors = col_bin(c( "#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1.5,1000), pretty = F)
        ) %>%
        data_color(
            columns = c(`7-day average daily TPR`),
            colors = col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.05, 1), pretty = F, na.color = "#e8e8e8")
        ) %>%
        # highlight national estimate
        tab_style(
            style = cell_fill(color = "#fcf8d4"),
            locations = cells_body(
                rows = Location == "India")
        )
    
    message("making cumulative table...")
    cumulative <- tib %>%
        select(-`# daily new cases`, -`# daily new deaths`, -`7-day average daily TPR`,
               -`7-day average daily CFR`, -R, -`daily tests`, -`daily vaccine doses`) %>%
        gt() %>%
        # format table body text
        tab_style(
            style     = cell_text(size = px(14), font = "helvetica"),
            locations = cells_body()
        ) %>%
        tab_style(
            style     = cell_text(weight = "bold"),
            locations = cells_body(c(Location))
        ) %>%
        # format column names
        tab_style(
            style = cell_text(
                size      = px(12),
                color     = "#999",
                font      = "helvetica",
                transform = "uppercase"
            ),
            locations = cells_column_labels(everything())
        ) %>%
        # format numbers
        fmt_number(
            columns  = c(CFR, TPR),
            decimals = 3
        ) %>%
        # random formatting
        tab_options(
            column_labels.border.top.style    = "none",
            column_labels.border.bottom.width = 1,
            column_labels.border.bottom.color = "#334422",
            table_body.border.bottom.color    = "#0000001A",
            data_row.padding                  = px(4)
        ) %>%
        # column widths
        cols_width(
            (Location) ~ px(150),
            (CFR) ~ px(75),
            everything() ~ px(100)
        ) %>%
        cols_align(
            align   = "center",
            columns = everything()
        ) %>%
        # title
        tab_header(
            title    = md("**Assessing COVID-19 in India**"),
            subtitle = glue("as of {format(today, '%B %e')}")
        ) %>%
        # caption
        tab_source_note(
            source_note = md(source_note_text)
        ) %>% 
        tab_spanner(
            label   = "Cumulative metrics",
            columns = c(`total cases`, `total deaths`, `TPR`, CFR,
                        `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
        ) %>% 
        cols_move_to_start((Location)) %>%
        tab_style(
            style = cell_text(
                size      = px(14),
                color     = "#999",
                font      = "helvetica",
                transform = "uppercase"
            ),
            locations = cells_column_spanners(spanners = c("Cumulative metrics")) 
        ) %>%
        # adjust title font
        tab_style(
            style     = list(cell_text(font = "helvetica", size = px(24))),
            locations = list(cells_title(groups = "title"))
        ) %>%
        # adjust subtitle font
        tab_style(
            style     = list(cell_text(font = "helvetica", size = px(18))),
            locations = list(cells_title(groups = "subtitle"))
        ) %>% 
        # highlight national estimate
        tab_style(
            style = cell_fill(color = "#fcf8d4"),
            locations = cells_body(rows = Location == "India")
        )
    
    message("outputting...")
    list(full = tabl,
         point_in_time = point_in_time,
         cumulative = cumulative)
    
}

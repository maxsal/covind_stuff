max_date   <- Sys.Date() - 1
d <- covid19bharat_ts()[date <= max_date]
# d          <- covid19india::get_all_data()[date <= max_date]
f          <- unique(d[, place])
f          <- f[!grepl("\\*", f)]
d          <- d[place %in% f]
dbc        <- daily_barplot_colors[!names(daily_barplot_colors) %in% "Cases"]

# load functions ----------
cli::cli_alert_info("loading functions")
fns <- list.files("fn")
walk(fns, ~source(paste0("fn/", .x)))

# data -----------
cli::cli_alert_info("prepping data")
datas <- purrr::map(f, prep_data) |>
  purrr::map(melt_data)
# vax_data <- get_clean_vax_data()[place %in% f]

# vax_max_date <- vax_data[, max(date)]

# check directories ----------
cli::cli_alert_info("checking directories")
if (!dir.exists(glue("bar_plots/{max_date}/"))) {
  dir.create(glue("bar_plots/{max_date}/"))
}

if (!dir.exists(glue("stack_plots/{max_date}"))) {
  dir.create(path = glue("stack_plots/{max_date}"), recursive = T)
}

if (!dir.exists(glue("vax_plots/{max_date}"))) {
  dir.create(glue("vax_plots/{max_date}"))
}

# plots ----------
options(warn = -1)

cli::cli_alert_info("bar plots")
inc_plots <- datas |>
  map2(f, inc_plot)

inc_plots |>
  walk2(f, save_bar_plot)

cli::cli_alert_info("r plots")
tvr_plots <- map(f, ~d[place == .x & date >= (max_date - 365)]) |>
  map2(f, tvr_plot)

stacked_plots <- map2(inc_plots, tvr_plots, ~.x / .y)

stacked_plots |>
  walk2(f, save_stack_plot)

cli::cli_alert_info("vax_plots")

vax_plots <- map(f, ~vax_bar_plot(data = d, state_name = .x))
vax_plots |>
  walk2(f, save_vax_plot)

options(warn = 1)  

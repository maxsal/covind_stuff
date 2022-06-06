quick_save <- function(x, date) {
  if (!dir.exists(glue("metrics_table/{date}/"))) {
    dir.create(glue("metrics_table/{date}/"))
  }
  
  gtsave(x$full, filename = glue("metrics_table/{date}/full_metrics_{format(vax_max_date, '%Y%m%d')}.png"), vheight = 800, vwidth = 1800)
  gtsave(x$point_in_time, filename = glue("metrics_table/{date}/point_in_time_{format(vax_max_date, '%Y%m%d')}.png"), vheight = 800, vwidth = 1000)
  gtsave(x$cumulative, filename = glue("metrics_table/{date}/cumulative_{format(vax_max_date, '%Y%m%d')}.png"), vheight = 800, vwidth = 1200)
}

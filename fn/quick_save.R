quick_save <- function(x, date, full_only = TRUE) {
  if (!dir.exists(glue("metrics_table/{date}/"))) {
    dir.create(glue("metrics_table/{date}/"))
  }
  
  if (full_only == FALSE) {
    gtsave(x$point_in_time, filename = glue("metrics_table/{date}/point_in_time_{format(date, '%Y%m%d')}.png"), vheight = 800, vwidth = 1000)
    gtsave(x$cumulative, filename = glue("metrics_table/{date}/cumulative_{format(date, '%Y%m%d')}.png"), vheight = 800, vwidth = 1200)
  }
  gtsave(x$full, filename = glue("metrics_table/{date}/full_metrics_{format(date, '%Y%m%d')}.png"), vheight = 800, vwidth = 1800)
  
}

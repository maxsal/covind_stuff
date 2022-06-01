save_bar_plot <- function(plot, state_name) {
  cairo_pdf(filename = glue("bar_plots/{max_date}/{max_date}_{state_name}_bar_plot.pdf"),
            width = 12, height = 6)
  print(plot)
  dev.off()
}
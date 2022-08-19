save_vax_plot <- function(plot, state_name) {
  cairo_pdf(filename = glue("vax_plots/{max_date}/{state_name}_vax_plot.pdf"),
            width = 9, height = 5)
  print(plot)
  dev.off()
}
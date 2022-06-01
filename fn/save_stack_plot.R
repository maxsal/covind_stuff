save_stack_plot <- function(plot, state_name) {
  cairo_pdf(filename = glue("stack_plots/{max_date}/{state_name}_plot.pdf"),
            width = 10, height = 8)
  print(plot)
  dev.off()
}
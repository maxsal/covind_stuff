ally::libri(tidyverse, data.table, glue, janitor, here, ally, patchwork, maxsal/covid19india)

d <- covid19india::get_all_data() %>%
  group_by(place) %>%
  arrange(date) %>%
  dplyr::mutate(
    smooth_daily_cases = zoo::rollmean(daily_cases, k = 7, align = "right", na.pad = TRUE)
  ) %>%
  ungroup()

start <- as.Date("2020-03-23")
end   <- as.Date("2020-08-31")

(r_plot <- d %>%
  dplyr::filter(place == "India" & date >= start & date <= end) %>%
  ggplot(aes(x = date, y = r_est)) +
  geom_hline(yintercept = 1, color = "#FF9933") +
  geom_ribbon(aes(ymin = r_lower, ymax = r_upper), fill = "#138808", alpha = 0.5) +
  geom_line(color = "#138808", size = 1) +
  geom_point(shape = 3, size = 0.25) +
  
  # national lockdown
  annotate(geom = "text", label = "India declares\nnational lockdown",
           x = as.Date("2020-04-05"), y = 4, color = "#138808",
           hjust = 0, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-04-04"), y = 4,
                   xend = as.Date("2020-03-24"), yend = 4.2),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  
  # lockdown extension
  annotate(geom = "text", label = "Lockdown extended",
           x = as.Date("2020-05-01"), y = 2.5, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-05-01"), y = 2.4,
                   xend = as.Date("2020-04-13"), yend = 1.85),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  geom_segment(aes(x = as.Date("2020-05-01"), y = 2.4,
                   xend = as.Date("2020-05-05"), yend = 1.75),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  
  # unlock 1
  annotate(geom = "text", label = "Unlock 1.0",
           x = as.Date("2020-06-01"), y = 1.75, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-06-01"), y = 1.65,
                   xend = as.Date("2020-06-01"), yend = 1.32),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  
  # unlock 2
  annotate(geom = "text", label = "Unlock 2.0",
           x = as.Date("2020-07-01"), y = 1.6, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-07-01"), y = 1.5,
                   xend = as.Date("2020-07-01"), yend = 1.26),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  
  # unlock 3
  annotate(geom = "text", label = "Unlock 3.0",
           x = as.Date("2020-07-25"), y = 1.7, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-07-25"), y = 1.6,
                   xend = as.Date("2020-07-25"), yend = 1.39),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  
  
  annotate(geom = "text", label = "Unlock 4.0",
           x = as.Date("2020-08-20"), y = 1.55, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-08-20"), y = 1.45,
                   xend = as.Date("2020-08-29"), yend = 1.2),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  
  scale_x_date(date_labels = "%B %e, %Y") +
  labs(
    title = "Estimated R in India",
    subtitle = glue("from {format(start, '%B %e, %Y')} to {format(end, '%B %e, %Y')}"),
    x = "Date",
    y = "Time-varying R",
    caption = "**\uA9 COV-IND-19 Study Group**"
  ) +
  theme_classic() +
  ggplot2::theme(
    text            = ggplot2::element_text(family = "Lato"),
    legend.position = "top",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )
)


(case_plot <- d %>%
  dplyr::filter(place == "India" & date >= start & date <= end) %>%
  ggplot(aes(x = date, y = smooth_daily_cases)) +
  geom_line(color = "#138808", size = 1) +
  geom_point(shape = 3, size = 0.25) +
  
  # national lockdown
  annotate(geom = "text", label = "India declares\nnational lockdown",
           x = as.Date("2020-04-01"), y = 20000, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-04-01"), y = 16000,
                   xend = as.Date("2020-03-24"), yend = 750),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +

  # # lockdown extension
  annotate(geom = "text", label = "Lockdown extended",
           x = as.Date("2020-05-01"), y = 13000, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-05-01"), y = 11000,
                   xend = as.Date("2020-04-13"), yend = 1300),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  geom_segment(aes(x = as.Date("2020-05-01"), y = 11000,
                   xend = as.Date("2020-05-05"), yend = 3000),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +

  # unlock 1
  annotate(geom = "text", label = "Unlock 1.0",
           x = as.Date("2020-06-01"), y = 25000, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-06-01"), y = 23000,
                   xend = as.Date("2020-06-01"), yend = 8250),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +

  # unlock 2
  annotate(geom = "text", label = "Unlock 2.0",
           x = as.Date("2020-07-01"), y = 40000, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-07-01"), y = 38000,
                   xend = as.Date("2020-07-01"), yend = 20000),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +

  # unlock 3
  annotate(geom = "text", label = "Unlock 3.0",
           x = as.Date("2020-07-15"), y = 60000, color = "#138808",
           hjust = 0.5, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-07-15"), y = 58000,
                   xend = as.Date("2020-07-25"), yend = 46000),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +


  annotate(geom = "text", label = "Unlock 4.0",
           x = as.Date("2020-08-20"), y = 70000, color = "#138808",
           hjust = 1, family = "Lato") +
  geom_segment(aes(x = as.Date("2020-08-21"), y = 70000,
                   xend = as.Date("2020-08-27"), yend = 71000),
               arrow = arrow(length = unit(1.5, "mm")),
               color = "#138808") +
  
  scale_x_date(date_labels = "%B %e, %Y") +
  labs(
    title = "Estimated R in India",
    subtitle = glue("from {format(start, '%B %e, %Y')} to {format(end, '%B %e, %Y')}"),
    x = "Date",
    y = "Time-varying R",
    caption = "**\uA9 COV-IND-19 Study Group**"
  ) +
  theme_classic() +
  ggplot2::theme(
    text            = ggplot2::element_text(family = "Lato"),
    legend.position = "top",
    legend.title    = ggplot2::element_blank(),
    plot.title      = ggplot2::element_text(face = "bold", hjust = 0),
    plot.caption    = ggtext::element_markdown(hjust = 0)
  )
)

cairo_pdf(here("lockdown", "fig", "stack_annotated_plot.pdf"),
          width = 12, height = 10)
r_plot / case_plot
dev.off()

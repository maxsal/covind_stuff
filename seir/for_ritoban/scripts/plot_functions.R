# reported daily cases
plot_daily_reported_cases <- function(stat = "positive_daily_reported", save = TRUE) {
  tmp_dat <- rbindlist(
    list(
      as.data.table(clean_r8)[section == stat][, dr := 8],
      as.data.table(clean_r10)[section == stat][, dr := 10],
      as.data.table(clean_r14)[section == stat][, dr := 14]
    ), fill = TRUE
  )[pred == 1] 
  
  tmp_plot <- tmp_dat[, dr := factor(dr, levels = c("14", "10", "8"))] |>
    ggplot(aes(x = date, y = mean, group = factor(dr), fill = factor(dr))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B %e") +
    labs(
      x = "Date",
      y = "Predicted reported case count",
      title = "SEIR predicted reported daily case counts under varying Dr"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
  
  if (save == TRUE) {
    ggsave(plot = tmp_plot, filename = "plots/daily_reported_cases.pdf",
           width = 8, height = 5, device = cairo_pdf)
    message("plot saved to /plots folder")
  }
  
  return(tmp_plot)
  
}

# unreported daily cases
plot_daily_unreported_cases <- function(stat = "unreported_daily", save = TRUE) {
  
  tmp_unrep <- rbindlist(
    list(
      as.data.table(clean_r8)[section == "unreported_daily"][, dr := 8],
      as.data.table(clean_r10)[section == "unreported_daily"][, dr := 10],
      as.data.table(clean_r14)[section == "unreported_daily"][, dr := 14]
    ), fill = TRUE
  )[pred == 1]
  
  tmp_fn <- rbindlist(
    list(
      as.data.table(clean_r8)[section == "false_negative_daily"][, dr := 8],
      as.data.table(clean_r10)[section == "false_negative_daily"][, dr := 10],
      as.data.table(clean_r14)[section == "false_negative_daily"][, dr := 14]
    ), fill = TRUE
  )[pred == 1]
  
  tmp_dat <- rbindlist(list(
    tmp_unrep,
    tmp_fn
  ))[, sum(mean), by = c("date", "dr")][]
  
  tmp_plot <- tmp_dat[, dr := factor(dr, levels = c("14", "10", "8"))] |>
    ggplot(aes(x = date, y = V1, group = factor(dr), fill = factor(dr))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B %e") +
    labs(
      x = "Date",
      y = "Predicted unreported daily case count",
      title = "SEIR predicted unreported daily case counts under varying Dr"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
  
  if (save == TRUE) {
    ggsave(plot = tmp_plot, filename = "plots/daily_unreported_cases.pdf",
           width = 8, height = 5, device = cairo_pdf)
    message("plot saved to /plots folder")
  }
  
  return(tmp_plot)
}

# reported daily deaths
plot_daily_reported_deaths <- function(stat = "death_daily_reported", save = TRUE) {
  tmp_dat <- rbindlist(
    list(
      as.data.table(clean_r8)[section == stat][, dr := 8],
      as.data.table(clean_r10)[section == stat][, dr := 10],
      as.data.table(clean_r14)[section == stat][, dr := 14]
    ), fill = TRUE
  )[pred == 1] 
  
  tmp_plot <- tmp_dat[, dr := factor(dr, levels = c("14", "10", "8"))] |>
    ggplot(aes(x = date, y = mean, group = factor(dr), fill = factor(dr))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B %e") +
    labs(
      x = "Date",
      y = "Predicted reported death count",
      title = "SEIR predicted reported daily death counts under varying Dr"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
  
  if (save == TRUE) {
    ggsave(plot = tmp_plot, filename = "plots/daily_reported_deaths.pdf",
           width = 8, height = 5, device = cairo_pdf)
    message("plot saved to /plots folder")
  }
  
  return(tmp_plot)
  
}

# unreported daily deaths
plot_daily_unreported_deaths <- function(stat = "death_unreported", save = TRUE) {
  tmp_dat <- rbindlist(
    list(
      as.data.table(clean_r8)[section == stat][, dr := 8],
      as.data.table(clean_r10)[section == stat][, dr := 10],
      as.data.table(clean_r14)[section == stat][, dr := 14]
    ), fill = TRUE
  )[pred == 1] 
  
  tmp_plot <- tmp_dat[order(date), deaths := mean - shift(mean), by = dr][!is.na(deaths)][, dr := factor(dr, levels = c("14", "10", "8"))] |>
    ggplot(aes(x = date, y = deaths, group = factor(dr), fill = factor(dr))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B %e") +
    labs(
      x = "Date",
      y = "Predicted unreported death count",
      title = "SEIR predicted unreported daily death counts under varying Dr"
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
  
  if (save == TRUE) {
    ggsave(plot = tmp_plot, filename = "plots/daily_unreported_deaths.pdf",
           width = 8, height = 5, device = cairo_pdf)
    message("plot saved to /plots folder")
  }
  
  return(tmp_plot)
  
}
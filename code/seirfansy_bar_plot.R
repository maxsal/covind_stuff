library(data.table)
library(ggplot2)
library(ggtext)
library(glue)
library(patchwork)
library(here)

today <- Sys.Date() - 1

dat <- fread(glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{today}/seirfansy/prediction_tt.txt"),
             showProgress = FALSE)

cols <- c(
  "Observed" = "#138808",
  "Predicted" = "#FF9933"
)

tmp <- dat[section == "positive_daily_reported" & pred == 1 & date <=today + 30]

case_plot <- dat[section == "positive_daily_reported" & pred == 1 & date <=today + 30] |>
  ggplot(aes(x = date, y = mean)) +
  geom_bar(stat = "identity", fill = cols[2]) +
  labs(
    x     = "Date",
    y     = "Daily new cases",
    title = "Cases"
  )

death_plot <- dat[section == "death_daily_reported" & pred == 1 & date <=today + 30] |>
  ggplot(aes(x = date, y = mean)) +
  geom_bar(stat = "identity", fill = cols[1]) +
  labs(
    x     = "Date",
    y     = "Daily deaths",
    title = "Deaths"
  )

patch <- case_plot / death_plot

full_plot <- patch  +
  plot_annotation(
    title    = "SEIRfansy-projected daily COVID-19 case and death counts in India",
    subtitle = glue("{format(min(tmp$date), '%B %e, %Y')} to {format(max(tmp$date), '%B %e, %Y')}"),
    tag_levels = c("A")
  ) &
  theme(
    text              = element_text(family = "Lato"),
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 14, hjust = 0, color = "gray40"),
    plot.caption      = element_markdown(size = 12, hjust = 0),
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Lato", face = "bold")
  )

cairo_pdf(here("fig", glue("seirfansy_bar_{today}.pdf")), width = 8, height = 8)
full_plot
dev.off()

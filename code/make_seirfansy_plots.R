ally::libri(tidyverse, covid19india, scales, glue, here, ggtext)

today <- Sys.Date() - 1

if (!dir.exists(here("seirfansy_plots", today))) {
  dir.create(here("seirfansy_plots", today))
}

pred_days     <- 45
trailing_days <- 100

obs <- get_state_counts(keep_nat = TRUE) %>%
  filter(date >= (today - trailing_days) & date <= today) %>%
  select(place, date, value = daily_cases) %>%
  mutate(pred = "Observed")

cols <- c(
  "Observed" = "#138808",
  "Predicted" = "#FF9933"
)

places <- obs %>% pull(place) %>% unique()

for (i in seq_along(places)) {
  
  state <- places[i]
  cli::cli_alert(state)
  abb <- covid19india::pop %>% filter(place == state) %>% pull(abbrev)
  if (length(abb) > 1) {abb <- "ct"}
  
  if (!httr::http_error(glue::glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/2021-07-28/seirfansy/prediction_{abb}.txt"))) {
  tmp_plt <- data.table::fread(glue::glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/2021-07-28/seirfansy/prediction_{abb}.txt")) %>%
    filter(date >= today + 1 & date <= today + pred_days & section == "positive_daily_reported") %>%
    select(date, value = mean) %>%
    mutate(pred = "Predicted") %>%
    bind_rows(obs %>% filter(place == state) %>% select(-place)) %>%
    ggplot(aes(x = date, y = value)) +
    geom_line(aes(color = as.factor(pred)),size = 1) +
    geom_point(shape = 3, size = 0.25) +
    scale_color_manual(values = cols) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title    = paste0("Reported daily case counts for ", state),
      subtitle = paste0("from ", format(today - trailing_days, "%B %e, %Y"), " to ", format(today + pred_days, "%B %e, %Y")),
      x        = "Date",
      y        = "Reported daily case count",
      caption  = glue("**Note:** Predicting {pred_days} days forward<br>**\uA9 COV-IND-19 Study Group**")
    ) +
    theme_classic() +
    theme(
      text = element_text(family = "Lato"),
      plot.title = element_text(face = "bold"),
      plot.caption = element_markdown(hjust = 0),
      legend.position = "top",
      legend.title = element_blank()
    )
  
  tmp_plt
  
  ggsave(plot = tmp_plt,
         filename = here("seirfansy_plots", today, glue("{state}_pred_{today}.pdf")),
         width = 7, height = 5,
         device = cairo_pdf)
  } else {
    next
  }

}

# remotes::install_gitlab("maxsal/ally")
ally::libri(tidyverse, covid19india, data.table, scales, glue, ggtext)

# specs ----------
colores <- c(
  "Recovered"  = "#138808",
  "New cases"  = "#f2c82e",
  "Fatalities" = "#ED553B"
)

# data ---------
d <- get_nat_counts()

d <- melt(d,
             id.vars       = c("place", "date"),
             measure.vars  = c("daily_cases", "daily_deaths", "daily_recovered"),
             variable.name = "type",
             value.name    = "count")[]
d <- d[type == "daily_cases", type := "New cases"]
d <- d[type == "daily_deaths", type := "Fatalities"]
d <- d[type == "daily_recovered", type := "Recovered"]
d <- d[, type := factor(type, levels = c("New cases", "Fatalities", "Recovered"))]

# SOURCES: -----------
# Alpha: September 2020 in Kent, UK: https://www.nejm.org/doi/full/10.1056/NEJMc2103227
# Delta: October 2020 in India: https://www.nature.com/articles/s41586-021-03777-9
# Omicron: November 24, 2021 in South Africa: https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/scientific-brief-omicron-variant.html

# plot ----------
(plot <- d %>%
  ggplot(aes(x = date, y = count)) +
  
  annotate("segment", x = as.Date("2020-09-15"), xend = as.Date("2020-09-15"),
           y = d[date == "2021-09-15", sum(count)], yend = d[date == "2021-09-15", sum(count)] + 400000,
           linetype = 2, color = "gray40") +
  annotate("text", label = "September 2020\nAlpha variant first\ndetected in UK",
           x = as.Date("2020-09-10"), y = d[date == "2021-09-15", sum(count)] + 400000, vjust = 1, hjust = 1) +
  
  annotate("segment", x = as.Date("2020-10-15"), xend = as.Date("2020-10-15"),
           y = d[date == "2021-10-15", sum(count)], yend = d[date == "2021-10-15", sum(count)] + 400000,
           linetype = 2, color = "gray40") +
  annotate("text", label = "October 2020\nDelta variant first\ndetected in India",
           x = as.Date("2020-10-20"), y = d[date == "2021-10-15", sum(count)] + 400000, vjust = 1, hjust = 0) +
  
  annotate("segment", x = as.Date("2021-11-24"), xend = as.Date("2021-11-24"),
           y = d[date == "2021-11-24", sum(count)], yend = d[date == "2021-11-24", sum(count)] + 400000,
           linetype = 2, color = "gray40") +
  annotate("text", label = "November 24, 2021\nOmiron variant first\ndetected in South Africa",
           x = as.Date("2021-11-19"), y = d[date == "2021-11-24", sum(count)] + 400000, vjust = 1, hjust = 1) +
  
  geom_col(aes(fill = factor(type, c("Recovered", "Fatalities", "New cases")))) +
  scale_fill_manual(values = colores) +
   scale_x_date(date_labels = "%B %Y", date_breaks = "3 months") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    subtitle = glue("{format(d[, min(date)], '%B %e, %Y')} to {format(d[, max(date)], '%B %e, %Y')}"),
    x = "Date",
    y = "Daily counts",
    title = "Daily number of new COVID-19 cases, fatalities, and recovered in India",
    caption = glue(
      "**Data source:** covid19india.org and Inidan Ministry of Health and Family Welfare via umich-cphds/cov-ind-19-data GitHub<br>",
      "**Alpha source:** Walker et al. 2021, *NEJM*, doi: 10.1056/NEJMc2103227<br>",
      "**Delta source:** Planas et al. 2020, *Nature*, doi: 10.1038/s41586-021-03777-9<br>",
      "**Omicron source:** Science Brief: Omicron (B.1.1.529) Variant, *CDC*, https:&#47;&#47;www&#46;cdc.gov/coronavirus/2019-ncov/science/science-briefs/scientific-brief-omicron-variant.html"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    text = element_text(family = "Noto Sans"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_markdown(hjust = 0)
  )
)


ggsave(
  filename = "~/Downloads/plot_with_place.pdf",
  plot = plot,
  height = 5, width = 12, device = cairo_pdf
)

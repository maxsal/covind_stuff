# libraries and such -----------
pacman::p_load(tidyverse, here, glue, patchwork, ggtext, janitor)
source(here("lockdown", "extract_cfr.R"))

cols_1 <- c(
  "Strong effect"   = "#138808",
  "Moderate effect" = "#FF9933"
)

cols_2 <- c(
  "Moderate CFR" = "#138808",
  "High CFR"     = "#FF9933",
  "Low CFR"      = "#000080"
)

# load data ----------
pis <- read_tsv(here("data", "pi_schedule.txt"),
                col_types = cols()) %>%
  mutate(
    place = case_when(
      place == "India" ~ "Strong effect",
      place == "Maharashtra" ~ "Moderate effect"
    )
  )

cfrs <- extract_cfr() |>
  select(date, India = cfr_t7, Maharashtra = cfr_mh_t7,
         Kerala = cfr_kl_t7) |>
  pivot_longer(
    names_to = "Location",
    values_to = "CFR",
    -date
  ) %>%
  filter(date <= "2021-05-15") %>%
  mutate(
    Location = case_when(
      Location == "India" ~ "Moderate CFR",
      Location == "Maharashtra" ~ "High CFR",
      Location == "Kerala" ~ "Low CFR"
    )
  )

# make plots ----------
pi_plt <- pis %>%
  ggplot(aes(x = nseq, y = smooth_pis, group = place, color = place)) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
  geom_line(size = 1) +
  labs(
    title   = "Lockdown intervention schedules",
    x       = "Days since start of intervention",
    y       = "Pi(t)",
    caption = "Note: Dashed line represents no change to pi schedule"
  ) +
  scale_color_manual(values = cols_1) +
  theme_classic() +
  theme(
    text            = element_text(family = "Lato"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9, face = "bold"),
    axis.title.y    = element_text(size = 9, face = "bold"),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9, face = "bold"),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  )

cfr_plt <- cfrs %>%
  ggplot(aes(x = date, y = CFR, group = Location, color = Location)) +
  geom_line(size = 1) +
  labs(
    title   = "CFR schedules",
    x       = "Date",
    y       = "Case-fatality rate (CFR)",
    caption = "Note: February 15, 2021 to May 15, 2021"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%B") +
  scale_color_manual(values = cols_2) +
  theme_classic() +
  theme(
    text            = element_text(family = "Lato"),
    axis.text.x     = element_text(size = 9, vjust = 0.5),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 9, face = "bold"),
    axis.title.y    = element_text(size = 9, face = "bold"),
    legend.title    = element_blank(),
    legend.text     = element_text(size = 9, face = "bold"),
    legend.position = "top",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.subtitle   = element_text(size = 9, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 8, hjust = 0)
  )

# make figure ----------
patched <- pi_plt + cfr_plt

full_plt <- patched +
  plot_annotation(
    title    = "Illustration of intervention and CFR schedules",
    caption  = glue("**Abbrev:** CFR, case-fatality rate<br>",
                    "**\uA9 COV-IND-19 Study Group**"),
    tag_levels = c("A")
  ) &
  theme(
    text              = element_text(family = "Lato"),
    plot.title        = element_text(size = 14, face = "bold"),
    plot.subtitle     = element_text(size = 12, hjust = 0, color = "gray40"),
    plot.caption      = element_markdown(size = 8, hjust = 0),
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 14, hjust = 0, vjust = 1, family = "Lato", face = "bold")
  )

ggsave(filename = here("lockdown", "fig", "fig_schedules.pdf"),
       plot     = full_plt,
       height   = 5,
       width    = 10,
       units = "in", device = cairo_pdf)

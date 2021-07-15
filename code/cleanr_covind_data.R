pacman::p_load("tidyverse", "EpiEstim", "gt", "glue", "lubridate", "janitor",
    "scales", "ggtext", "here", "httr")
pacman::p_load_gh("maxsal/covid19india")

tabs   <- get_metrics_tables()

tabs$full
tabs$point_in_time
tabs$cumulative


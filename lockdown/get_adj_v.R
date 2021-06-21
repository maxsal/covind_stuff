get_adj_v <- function(scen, base_path = NULL, adj_len = 2, N = 1.34e9) {
  
  if (is.null(base_path)) {
    base_path <- "/Users/maxsalvatore/local/science_covind/main"
    message(paste0("using base_path: ", base_path))
  }
  
  tmp <- get(load(glue("{base_path}/{scen}_smooth1_plot_data.Rdata")))
  
  dataf <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv", col_types = cols()) %>%
    janitor::clean_names() %>%
    filter(date_ymd <= (as.Date(scen) - 1) & date_ymd >= (as.Date(scen) - 100)) %>%
    pull(total_confirmed)
  
  other_plot <- tmp$plot_data_ls[[2]]
  T_prime    <- tmp$other_plot[[1]]
  
  infection_plot_ls <- tmp$plot_data_ls[[4]]
  data_comp         <- tmp$infection_plot_ls[[3]]
  
  removed_plot_ls <- tmp$plot_data_ls[[5]]
  data_comp_R     <- tmp$removed_plot_ls[[3]]
  
  adj_v <- mean(
    as.vector(dataf[(T_prime-adj_len):T_prime])/
      N / 
      (data_comp[(T_prime-adj_len):T_prime,"mean"] + data_comp_R[(T_prime-adj_len):T_prime,"mean"])
    )
  
  return(adj_v)
  
}

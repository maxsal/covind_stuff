
dirs <- c("metrics_table/", "stack_plots/", "vax_plots/")

for (i in seq_along(dirs)) {
  
  flist <- list.files(dirs[i])
  
  flist_rm <- flist[!(as.Date(flist) %in% (Sys.Date() - 7):Sys.Date())]
  
  for (j in seq_along(flist_rm)) {
    unlink(paste0(dirs[i], flist_rm[j]), recursive = TRUE)
  }
  
}

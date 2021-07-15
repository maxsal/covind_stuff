check_for_deps <- function() {
 
  nzchar(system.file(package = "cli")) & nzchar(system.file(package = "remotes"))
   
}

check_package_installed <- function(x) {
  
  nzchar(system.file(package = x))
  
}

check_library_loaded <- function(x) {
  
  x %in% (.packages())
  
}

libri <- function(...) {
  
  if (check_for_deps() == FALSE) {
    stop("`remotes` and `cli` packages are required: `install.packages(c('remotes', 'cli'))`")
  }
  
  # list of libraries ----------
  libs      <- as.character(eval(substitute(alist(...))))
  preloaded <- 0
  installed <- 0
  newloads  <- 0
  
  preloaded_libs <- NULL
  installed_libs <- NULL
  newload_libs   <- NULL
  
  for (i in seq_along(libs)) {
    
  tmp_lib <- sub(".*/", "", libs[i])
    
  # check install -----------
  if (check_package_installed(tmp_lib) == FALSE) {
    
    if (grepl("/", libs[i]) == TRUE) {
      remotes::install_github(libs[i])
    } else {
      install.packages(libs[i])
    }
    installed <- installed + 1
    
    if (is.null(installed_libs)) {
      installed_libs <- libs[i]
    } else {
      installed_libs <- c(installed_libs, libs[i])
    }

  }
    
  # check loaded ----------
    
    if (check_library_loaded(tmp_lib) == TRUE) {
      
      preloaded      <- preloaded + 1
      if (is.null(preloaded_libs)) {
        preloaded_libs <- libs[i]
      } else {
        preloaded_libs <- c(preloaded_libs, libs[i])
      }
    }
    
  # load unloaded libraries ----------
    
    if (check_library_loaded(tmp_lib) == FALSE) {
      
      suppressPackageStartupMessages(library(tmp_lib, character.only = T))
      newloads     <- newloads + 1
      if (is.null(newload_libs)) {
        newload_libs <- tmp_lib
      } else {
        newload_libs <- c(newload_libs, tmp_lib)
      }
      
    }
    
  }
  
  # output summary ----------
  cli::cli_h1("Summary")
  if (preloaded > 0) cli::cli_alert_info("{preloaded} librar{?y/ies} already loaded: {paste(preloaded_libs, collapse = ', ')}")
  if (installed > 0) cli::cli_alert_info("{installed} librar{?y/ies} installed: {paste(installed_libs, collapse = ', ')}")
  if (newloads > 0) cli::cli_alert_success("{newloads} librar{?y/ies} loaded: {paste(newload_libs, collapse = ', ')}")
  
}

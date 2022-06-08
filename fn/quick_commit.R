quick_commit <- function(mes = "push plots", push_only = FALSE) {
  
  if (push_only == FALSE) {
    git2r::add(repo = ".", path = ".", force = FALSE)
    git2r::commit(repo = ".", message = "push plots")
  }
  cli::cli_alert_info("pushing to github")
  system("git push")
  cli::cli_alert_info("pushing to gitlab")
  system("git push gitlab")
  # git2r::push(name = "origin", refspec = "refs/heads/main")
  # git2r::push(name = "gitlab", refspec = "refs/heads/main")
}

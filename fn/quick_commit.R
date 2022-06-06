quick_commit <- function() {
  git2r::add(repo = ".", path = ".", force = FALSE)
  git2r::commit(repo = ".", message = "push plots")
  git2r::push(name = "origin", refspec = "refs/heads/main")
  git2r::push(name = "gitlab", refspec = "refs/heads/main")
}

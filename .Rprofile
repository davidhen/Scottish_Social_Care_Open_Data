## This makes sure that R loads the workflowr package
## automatically, everytime the project is loaded
if (requireNamespace("workflowr", quietly = TRUE)) {
  message("Loading .Rprofile for the current workflowr project")
  library("workflowr")
} else {
  message("workflowr package not installed, please run devtools::install_github('jdblischak/workflowr') to use the workflowr functions")
}
options(prompt="R> ",
        show.signif.stars=FALSE,
        scipen = 9)


#--------------------------------------------------------------------------------------
# The following helper functions were adopted from the 'thesisdown' package
#--------------------------------------------------------------------------------------

#' @importFrom R.utils copyDirectory
#'
# From thesisdown package and https://github.com/rstudio/bookdown/blob/master/R/utils.R
UHHthesis_file <- function(...) {
  system.file(..., package = 'UHHthesis', mustWork = TRUE)
}


# From thesisdown package and https://github.com/rstudio/bookdown/blob/master/R/skeleton.R
UHHthesis_skeleton <- function(path = getwd()) {

  # copy 'resources' folder to path
  resources <- UHHthesis_file('rstudio', 'templates', 'project', 'resources')

  R.utils::copyDirectory(
    from = resources,
    to = path,
    recursive = TRUE
  )
}



#make pdf/html from a .Rmd file
#' @export
render <- function(file, quiet) rmarkdown::render(file, quiet=quiet)


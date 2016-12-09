
# make pdf/html from a .Rmd file
# CE: made this an internal function  # ' @export
# This function just calls the corresponding function from rmarkdown
render <- function(file, quiet) rmarkdown::render(file, quiet=quiet)


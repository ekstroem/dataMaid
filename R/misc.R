#Miscellaneous methods that do not belong in any specfic
#other file. Only methods are allowed in this file, other
#functions, however minor, should have their own files.

#Print overview of all functions of a certain  type, used in 
#allVisualFunctions(), allSummaryFunctions(), 
#allCheckFunctions().
#' @importFrom pander pander
#' @export
print.functionSummary <- function(x, ...) {
  x$classes <- sapply(x$classes, function(x) paste(x, collapse=", "))
  pander::pander(data.frame(x, row.names = NULL), justify="left")
}


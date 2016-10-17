#'
#'document me!
#'
#' @export
summaryFunction <- function(f, description, classes = NULL) {
  f <- deparse(substitute(f))
  makeXFunction(f, description, classes, "summaryFunction")
}


#' document me!
#' @export
allSummaryFunctions <- function() {
  allXFunctions("summaryFunction")
}



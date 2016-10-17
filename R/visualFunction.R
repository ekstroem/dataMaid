#'
#' Visual funtction
#'
#' document me!
#'
#' @export
visualFunction <- function(f, description, classes = NULL) {
  f <- deparse(substitute(f))
  makeXFunction(f, description, classes, "visualFunction")
}


#'
#' document me!
#'
#' @export
allVisualFunctions <- function() {
  allXFunctions("visualFunction")
}



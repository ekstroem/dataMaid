
#' document me!
#' @export
checkFunction <- function(f, description, classes=NULL) {
  f <- deparse(substitute(f))
  makeXFunction(f, description, classes, "checkFunction")
}

#to do: change it such that a checkFunction is constructed e.g. like
# foo <- checkFunction(.description, x) {
#   x + 2
#}
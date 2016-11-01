
#' @export
classes <- function(x) UseMethod("classes")

#' @export
classes.default <- function(x) NULL

#' @export
classes.checkFunction <- function(x) attr(x, "classes")

#' @export
classes.summaryFunction <- function(x) attr(x, "classes")

#' @export
classes.visualFunction <- function(x) attr(x, "classes")

#' @export
`classes<-` <- function(x, value) {
  attr(x, "classes") <- value
  x
}
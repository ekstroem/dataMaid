#' Extract the contents of the attribute \code{classes}
#'
#' If the object, \code{x}, is itself of
#' class \code{\link{checkFunction}}, \code{\link{summaryFunction}}
#' or \code{\link{visualFunction}}, the contents of \code{x}'s
#' attribute \code{classes} is returned. Otherwise, \code{NULL} is
#' returned.
#'
#' @param x The object for which the \code{classes}
#' attribute should be extracted.
#'
#' @return The classes for which \code{x} is intended to be called,
#' given as a vector of characters.
#'
#' @examples
#' #Extract the classes of the checkFunction identifyMissing
#' classes(identifyMissing)
#'
#' #Extract the classes of the summaryFunction minMax
#' classes(minMax)
#'
#' #Extract the classes of the visualFunction basicVisual
#' classes(basicVisual)
#'
#' @include minMax.R identifyMissing.R basicVisual.R
#'
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


#' @rdname classes
#' @usage classes(x) <- value
#' @param value New value
#' @export classes<-
`classes<-` <- function(x, value) {
  attr(x, "classes") <- value
  x
}

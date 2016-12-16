#' @title Extract the contents of the attribute \code{classes}
#' 
#' @usage 
#' classes(x) 
#' classes(x) <- value
#' 
#' @description If the object, \code{x}, is itself of 
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


# I have no idea what to do here... 
#' 
#' @title Get or set the \R{classes} attribute of an object
#' 
#' @description ???????? Required for document() not to complain
#' 
#' @details ??????????? Required for document() not to complain
#' 
#' @export
`classes<-` <- function(x, value) {
  attr(x, "classes") <- value
  x
}
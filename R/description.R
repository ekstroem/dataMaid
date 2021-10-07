#' Extract the contents of the attribute \code{description}
#'
#' If the object, \code{x}, is itself of
#' class \code{\link{checkFunction}}, \code{\link{summaryFunction}}
#' or \code{\link{visualFunction}}, the contents of \code{x}'s
#' attribute \code{description} is returned. Otherwise, \code{NULL} is
#' returned.
#'
#' @param x The object for which the \code{description}
#' attribute should be extracted.
#'
#' @return A description of what \code{x} does, given as
#' a character string.
#'
#' @examples
#' #Extract the description of the checkFunction identifyMissing
#' description(identifyMissing)
#'
#' #Extract the description of the summaryFunction minMax
#' description(minMax)
#'
#' #Extract the description of the visualFunction basicVisual
#' description(basicVisual)
#'
#' @include minMax.R identifyMissing.R basicVisual.R
#'
#' @export
description <- function(x) UseMethod("description")

#' @export
description.default <- function(x) deparse(substitute(x))

#' @export
description.checkFunction <- function(x) attr(x, "description")

#' @export
description.summaryFunction <- function(x) attr(x, "description")

#' @export
description.visualFunction <- function(x) attr(x, "description")


#' @rdname description
#' @usage description(x) <- value
#' @param value New value
#' @export description<-
`description<-` <- function(x, value) {
  attr(x, "description") <- value
  x
}

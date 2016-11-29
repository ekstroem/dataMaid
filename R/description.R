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

#' @export
`description<-` <- function(x, value) {
  attr(x, "description") <- value
  x
}
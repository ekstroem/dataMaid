#' @export
oClass <- function(v) UseMethod("oClass")
oClass.default <- function(v) class(v)
oClass.smartNum <- function(v) attr(v, "originalClass")

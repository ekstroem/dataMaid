#"unpack" a vector of class labelled (Wickham) by concatenating the code values
#with the labels

#maybe delete? Deal when labelled stuff is implemented
#' @export
unpackLabelled <- function(v) {
  c(as.character(v), attributes(v)$labels)
}






#' @importFrom pander pander
#' @export
print.functionSummary <- function(x, ...) {
  x$classes <- sapply(x$classes, function(x) paste(x, collapse=", "))
  pander(data.frame(x, row.names = NULL), justify="left")
}





allVarClasses <- function() {
  c("character", "factor", "labelled", "numeric", "integer", "logical")
}
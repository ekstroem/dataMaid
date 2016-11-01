#' @export
smartNum <- function(v) {
  oriClass <- class(v)
  v <- factor(v)
  attr(v, "originalClass") <- oriClass
  class(v) <- c("smartNum", "factor")
  v
}

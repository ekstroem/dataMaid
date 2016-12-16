#' @export
summaryResult <- function(ls) {
  if (length(setdiff(names(ls), c("feature", "result", "value"))) != 0) {
    stop("Hmm, what's informative here? Describe issue...")
  }
  class(ls) <- "summaryResult"
  ls
}

#' @export
print.summaryResult <- function(x, ...) {
  value <- x$value
  if (is.numeric(value)) {
    value <- round(value, 4)
  }
  mes <- paste(x$feature, ": ",
               paste(value, collapse = ", "),
               sep="")
  cat(mes)
}

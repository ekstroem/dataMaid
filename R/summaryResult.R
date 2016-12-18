#' @title Create object of class summaryResult
#'
#' @description Convert a list resulting from the summaries performed in a
#' \code{\link{summaryFunction}} into a \code{summaryResult} object, thereby
#' supplying it with a \code{print()} method.
#'
#' @param ls A list with entries \code{$feature} (a character string describing
#' what summary was obtained), \code{$result} (the result of the summary, either
#' a value from the variable, a numeric or a character string) and 
#' \code{$value} (the result in its most raw format, often identical to the 
#' \code{$result} input). 
#'
#' @return A S3 object of class \code{summaryResult}, identical to the inputted
#' list, \code{ls}, except for its class attribute.
#'
#' @seealso \code{\link{summaryFunction}}
#'
#' @export
summaryResult <- function(ls) {
  if (length(setdiff(names(ls), c("feature", "result", "value"))) != 0) {
    stop("The inputted list does not qualify as a summaryResult")
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

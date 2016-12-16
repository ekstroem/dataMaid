#' Create object of class summaryResult
#'
#' Covert a list resulting from the summaries performed in a
#' \code{\link{summaryFunction}} into a \code{summaryResult} object, thereby
#' supplying it with a \code{print()} method.
#'
#' @param ls A list with entries \code{$problem} (logical indicating whether
#' a problem was found), \code{$message} (a character string containing a
#' message describing the problem) and \code{$problemValues} (the values
#' in the checked variables that were marked as problematic). Note that
#' \code{$message} and \code{$problemValues} can be left empty (i.e.
#' \code{""} and \code{NULL}, respectively), if they are not relevant.  XXX
#'
#' @return A S3 object of class \code{summaryResult}, identical to the inputted
#' list, \code{ls}, except for its class attribute.
#'
#' @seealso \code{\link{summaryFunction}}
#'
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

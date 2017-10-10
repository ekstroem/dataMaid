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
  entryNames <- names(ls)
  if (length(setdiff(entryNames, c("feature", "result", "value"))) != 0) {
    stop("The inputted list does not qualify as a summaryResult")
  } 
  if (!("value" %in% entryNames) & !("result" %in% entryNames)) {
    stop("A summaryResult must have a $value slot or a $res slot")
  }
  if (!("result" %in% entryNames)) {
    if (is.numeric(ls$value)) {
      ls$result <- round(ls$value, 4)
    } else {
      ls$result <- ls$value
    }
    #make sure e.g. vectors and lists are collapsed into one character string
    ls$result <- paste(ls$result, collapse = " ") 
  }
  class(ls) <- "summaryResult"
  ls
}

#' @export
print.summaryResult <- function(x, ...) {
  mes <- paste(x$feature, ": ",
               paste(x$result, collapse = ", "),
               sep="")
  cat(mes)
}

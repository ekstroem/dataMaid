#' @title Create object of class checkResult
#' 
#' @description Convert a list resulting from the checks performed in a 
#' \code{\link{checkFunction}} into a \code{checkResult} object, thereby
#' supplying it with a \code{print()} method.
#' 
#' @param ls A list with entries \code{$problem} (logical indicating whether
#' a problem was found), \code{$message} (a character string containing a 
#' message describing the problem) and \code{$problemValues} (the values
#' in the checked variables that were marked as problematic). Note that
#' \code{$message} and \code{$problemValues} can be left empty (i.e. 
#' \code{""} and \code{NULL}, respectively), if they are not relevant.
#' 
#' @return A S3 object of class \code{checkResult}, identical to the inputted
#' list, \code{ls}, except for its class attribute. 
#' 
#' @seealso \code{\link{checkFunction}} 
#' 
#' @export
checkResult <- function(ls) {
  if (length(setdiff(names(ls), c("problem", "message", "problemValues"))) != 0) {
    stop("The inputted list does not qualify as a checkResult")
  } else {
    class(ls) <- "checkResult"
  }
  ls
}


#' @export
print.checkResult <- function(x, ...) {
  if (x$problem) {
    mes <- x$message
  } else mes <- "No problems found."
  
  #remove escaping and quoting designed for rmarkdown rendering
  mes <- gsub("\\\\\"", "", mes)
  
  cat(mes)
}
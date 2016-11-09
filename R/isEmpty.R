#' @title Check if a variable only contains a single value
#' @description A \code{\link{checkFunction}} that checks if \code{v} only contains a single unique value 
#' (possibly \code{NA}).
#' @param v A variable (vector) to check.
#' @return A list with two elements, $problem: \code{TRUE} if \code{v} contains only one value,
#' \code{FALSE} otherwise, and $message A message that either describes the issue (if $problem is \code{TRUE}) 
#' or is empty (otherwise).
#' 
#' @examples 
#' emptyVar <- rep("a", 10)
#' notEmptyVar <- c("a", "a", "b", "c", "d", "e", "f")
#' 
#' isEmpty(emptyVar)
#' isEmpty(notEmptyVar)
#' @export
isEmpty <- function(v) {
  out <- list(problem = FALSE, message = "")
  if (length(unique(v)) == 1) {
    out$problem <- TRUE
    out$message <- paste("The variable only takes one value: ", printProblemValues(v[1]),
                         ".", sep="")
  }
  out
}


#make it a checkFunction
isEmpty <- checkFunction(isEmpty, "Check if the variable contains only a single value", allVarClasses())


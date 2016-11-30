#' @title Check if a variable is a key in the dataset
#' @description A \code{\link{checkFunction}} that checks if \code{v} is a key, that is if every 
#' observation has a unique value in \code{v} and \code{v} is either a factor or character variable.
#' @param v A variable (vector) to check.
#' @return A list with two elements, $problem: \code{TRUE} if \code{v} is a key,
#' \code{FALSE} otherwise, and $message A message that either describes the issue (if $problem is \code{TRUE}) 
#' or is empty (otherwise).
#' @details Note that numeric or integer variables are not considered candidates for keys, as truly
#' continuous measurements will most likely result in unique values for each observation.
#' @examples 
#' keyVar <- c("a", "b", "c", "d", "e", "f")
#' notKeyVar <- c("a", "a", "b", "c", "d", "e", "f")
#' 
#' isKey(keyVar)
#' isKey(notKeyVar)
#' 
#' @export
isKey <- function(v) {
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  if (length(unique(v)) == length(v) & !any(class(v) %in% c("numeric", "integer"))) {
    out$problem <- TRUE
    out$message <- "The variable is a key (distinct values for each observation)." 
  }
  checkResult(out)
}


#make it a checkFunction
isKey <- checkFunction(isKey, "Check if the variable is a key", allVarClasses())

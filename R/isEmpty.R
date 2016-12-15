#' @title Check if a variable only contains a single value
#' @description A \code{\link{checkFunction}} that checks if \code{v} only contains a single unique value 
#' (possibly \code{NA}).NOT TRUE - UPDATE TEXT TO REFLECT WHAT WE DO WITH NAs
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
  lV <- length(v)
  v <- na.omit(v)
  pctMiss <- round(100*(lV - length(v))/lV, 2)
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  nVals <- length(unique(v))
  if (nVals <= 1) {
    allNA <- nVals == 0
    val <- ifelse(allNA, "NA", as.character(v[1]))
    out$problem <- TRUE
    out$message <- paste("The variable only takes one ",
                         ifelse(allNA, "", "(non-missing) "),
                         "value: ", printProblemValues(val), ".",
                         ifelse(allNA, "", 
                                paste(" The variable contains", 
                                      pctMiss, 
                                      "\\% missing observations.")),
                         sep="")
  }
  checkResult(out)
}


#make it a checkFunction
isEmpty <- checkFunction(isEmpty, "Check if the variable contains only a single value", allVarClasses())


#' @title Check if a variable only contains a single value
#' 
#' @description A \code{\link{checkFunction}} that checks if \code{v} only 
#' contains a single unique value, aside from missing values. This
#' function is intended for use as a precheck in \code{\link{makeDataReport}}.
#' 
#' @param v A variable (vector) to check. All variable types are allowed.
#' 
#' @return  A \code{\link{checkResult}} with three entires: 
#' \code{$problem} (a logical indicating whether \code{v} contains only one value), 
#' \code{$message} (if a problem was found, a message describing which single 
#' value the variable takes and how many missing observations it contains, otherwise 
#' ""), and \code{$problemValues} (always \code{NULL}).  
#'  
#' @examples 
#' singularVar <- c(rep("a", 10), NA, NA)
#' notSingularVar <- c("a", "a", "b", "c", "d", "e", "f", NA, NA)
#' 
#' isSingular(singularVar)
#' isSingular(notSingularVar)
#' 
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}}, 
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#' 
#' @importFrom haven as_factor
#' 
#' @export
isSingular <- function(v) {
  lV <- length(v)
  
  if ("labelled" %in% class(v)) v <- haven::as_factor(v) #otherwise na.omit does not work
  
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
#' @include allClasses.R allClasses.R
isSingular <- checkFunction(isSingular, 
                         "Check if the variable contains only a single value", 
                         allClasses())


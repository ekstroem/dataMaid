#' @title Check if a variable has a class supported by dataMaid
#' 
#' @description A \code{\link{checkFunction}} that checks if \code{v} has 
#' one of the classes supported by dataMaid, namely \code{character},
#' \code{factor}, \code{numeric}, \code{integer}, \code{labelled},
#' \code{logical} and \code{Date} (inlcuding other classes that inherits
#' from any of these classes). A user supported list can be provided
#' in the \code{treatXasY} argument, which will let the user decide
#' how unsupported classes should be treated. This
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
#' integerVar <- 1:10 #supported
#' rawVar <- as.raw(1:10) #not supported
#' 
#' isSupported(integerVar)
#' isSupported(rawVar)
#' 
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}}, 
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#' 
#' @export
isSupported <- function(v) {
  suppClasses <-  c("character", "factor", "labelled", "numeric", "integer", 
                    "logical", "Date")
  vClasses <- class(v)
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  if (any(vClasses %in% suppClasses)) {
    return(checkResult(out))
  }
  out$problem <- TRUE
  out$message <- paste("The variable has class", vClasses[1],
                       "which is not supported by dataMaid.")
  checkResult(out)
}


#make it a checkFunction
#' @include allClasses.R allClasses.R
isSupported <- checkFunction(isSupported, 
                         "Check if the variable class is supported by dataMaid.", 
                         allClasses())


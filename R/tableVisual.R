#' Produce tables for the makeDataReport visualizations.
#'
#' Produce a table of the distribution of a categorical (character, labelled, haven_labelled or factor) variable. 
#' Note that \code{tableVisual} is a \code{\link{visualFunction}}, compatible with the 
#' \code{\link{visualize}} and \code{\link{makeDataReport}} functions. 
#'
#' @param v The variable (vector) to be plotted.
#' @param vnam The name of the variable.
#' @param doEval If TRUE, the table itself is returned. Otherwise, the function returns
#' a character string containing standalone R code for producing the table.
#'
#' @examples
#' \dontrun{
#' #Save a variable
#' myVar <- c("red", "blue", "red", "red", NA) 
#' 
#' #Plot a variable
#' tableVisual(myVar, "MyVar")
#' 
#' #Produce code for plotting a variable
#' tableVisual(myVar, "MyVar", doEval = FALSE)
#' }
#' @seealso \code{\link{visualize}}, \code{\link{basicVisual}}, \code{\link{standardVisual}}
#'
#' @importFrom pander pander
#' @export
tableVisual <- function(v, vnam, doEval = TRUE) {
  x <- table(v, useNA = "always")
  x <- t(rbind(x, paste(round(rbind(x/length(v)),4)*100, "%", sep = "")))
  x <- cbind(dimnames(x)[[1]], x)
  rownames(x) <- NULL
  dimnames(x)[[2]] <- c("value", "count", "percentage")
  thisCall <- call("pander", x = x, keep.trailing.zeros = TRUE)
  if (!doEval) return(deparse(thisCall))
  else return(eval(thisCall))
}




#' @include visualFunction.R
tableVisual <- visualFunction(tableVisual, "Distribution tables",
                                 classes = c("character", "factor", "labelled", "haven_labelled"))


##########################################Not exported below#########################################

#v <- toyData$pill
#x <- table(v, useNA = "always")
#x <- t(rbind(x, paste(round(rbind(x/length(v)),4)*100, "%", sep = "")))
#dimnames(x)[[2]] <- c("count", "percentage")
#thisCall <- call("pander", x = x, caption = "vnam", keep.trailing.zeros = TRUE)
#eval(thisCall)

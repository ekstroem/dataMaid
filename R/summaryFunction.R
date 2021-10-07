#' @title Create an object of class summaryFunction
#' 
#' @description Convert a function, \code{f}, into an S3 
#' \code{summaryFunction} object. This adds \code{f} to the
#' overview list returned by an \code{allSummaryFunctions()} 
#' call.
#'
#' @inheritParams checkFunction
#' 
#' @param description A character string describing the summary
#' returned by \code{f}. If \code{NULL} (the default), the
#' name of \code{f} will be used instead.
#' 
#' @return A function of class \code{summaryFunction} which has to attributes, 
#' namely \code{classes} and \code{description}. 
#' 
#' @details \code{summaryFunction} represents the functions used in 
#' \code{\link{summarize}} and \code{\link{makeDataReport}} for summarizing the
#' features of variables in a dataset.
#' 
#' An example of defining a new \code{summaryFunction} is given below. 
#' Note that the minimal requirements for such a function (in order for it to be 
#' compatible with \code{summarize()} and \code{makeDataReport()}) is the following 
#' input/output-structure: It must input at least two arguments, namely 
#' \code{v} (a vector variable) and \code{...}. Additional implemented 
#' arguments from \code{summarize()} and \code{makeDataReport()} include
#' \code{maxDecimals}, see e.g. the pre-defined \code{summaryFunction} 
#' \code{\link{minMax}} for more details about how this arguments should
#' be used. 
#' The output must be a list with at least the two entries \code{$feature} 
#' (a short character string describing what was summarized) and \code{$result} 
#' (a value or a character string with the result of the summarization). 
#' However, if the result of a \code{summaryFunction} is furthermore 
#' converted to a \code{\link{summaryResult}} object, a \code{print()} 
#' method also becomes available for consistent formatting of 
#' \code{summaryFunction} results.
#' 
#' Note that all available \code{summaryFunction}s are listed by the call
#' \code{allSummaryFunctions()} and we recommed looking into these function,
#' if more knowledge about \code{summaryFunction}s is required.
#' 
#' @include makeXFunction.R allClasses.R
#' 
#' @seealso \code{\link{allSummaryFunctions}}, \code{\link{summarize}}, 
#' \code{\link{makeDataReport}}, \code{\link{checkResult}}
#' 
#' @examples 
#' 
#' #Define a valid summaryFunction that can be called from summarize() 
#' #and makeDataReport(). This function counts how many zero entries a given 
#' #variable has:
#'  countZeros <- function(v, ...) {
#'   res <- length(which(v == 0))
#'   summaryResult(list(feature = "No. zeros", result = res, value = res))
#'  }
#' 
#' #Convert it to a summaryFunction object. We don't count zeros for 
#' #logical variables, as they have a different meaning here (FALSE):   
#'  countZeros <- summaryFunction(countZeros, description = "Count number of zeros",
#'                              classes = setdiff(allClasses(), "logical"))
#'                              
#' #Call it directly :
#'  countZeros(c(0, 0, 0, 1:100))
#' 
#' #Call it via summarize():
#'  data(cars)
#'  summarize(cars, numericSummaries = c(defaultNumericSummaries(),
#'    "countZeros"))
#'
#' #Note that countZeros now appears in a allSummaryFunctions() call:
#'  allSummaryFunctions()
#' 
#' @export
summaryFunction <- function(f, description, classes = NULL) {
  f <- deparse(substitute(f))
  makeXFunction(f, description, classes, "summaryFunction")
}




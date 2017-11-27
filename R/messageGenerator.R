#' @title Produce a message for the output of a checkFunction
#' @description Helper function for producing output messages for
#' \code{\link{checkFunction}} type functions.
#'
#' @param problemStatus A list consisting of two entries:
#'
#' \code{$problem} - logical indicating whether a problem was found by the
#' \code{checkFunction} responsible for the making the \code{messageGenerator()} call,
#'
#' \code{$problemValues} - a vector of values from the variable that were
#' deemed problematic (see details below).
#'
#' @param message Optional, but recommended. A message describing what problem the
#' problem values are related to. If \code{NULL} a standard message is added using the name
#' of the function that called \code{messageGenerator}.
#'
#' @param nMax Maximum number of problem values to be printed in the message. If the total
#' number of problem values exceeds nMax, the number of omitted problem
#' values are added to the message. Defaults to \code{Inf}, in which case all problem
#' values are printed.
#'
#' @details This function is a tool for building \code{\link{checkFunction}}s for the
#' \code{dataMaid} \code{\link{makeDataReport}} function. \code{checkFunction}s will often identify a number
#' of values in a variable that are somehow problematic. \code{messageGenerator} takes
#' these values, pastes them together with a problem description and makes sure that the
#' formatting is appropriate for being rendered in a \code{rmarkdown} document.
#' We recommend writing short and precise problem descriptions (see examples),
#'  but if no message is supplied, the following message is generated:
#' "Note that a check function found the following problematic values: [problem values]".
#'
#' @return A character string with a problem description.
#'
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}, \code{\link{makeDataReport}}
#'
#' @examples
#'
#' #Varibales with/without underscores
#'  noUSVar <- c(1:10)
#'  USVar <- c("_a", "n_b", "b_", "_", 1:10)
#'
#' #Define a checkFunction using messageGenerator with a manual
#' #problem description:
#' identifyUnderscores <- function(v, nMax = Inf) {
#'   v <- as.character(v)
#'   underscorePlaces <- regexpr("_", v) > 0
#'   problemValues <- unique(v[underscorePlaces])
#'   problem <- any(underscorePlaces)
#'   message <- messageGenerator(list(problemValues = problemValues, problem = problem),
#'                               "The following values contain underscores:",
#'                               nMax = nMax)
#'   checkResult(list(problem = problem, message = message,
#'       problemValues = problemValues))
#'  }
#'
#'  identifyUnderscores(noUSVar) #no problem
#'  identifyUnderscores(USVar) #problems
#'  
#' #Only print the first two problemvalues in the message:
#'  identifyUnderscores(USVar, nMax = 2)
#'
#' #Define same function, but without a manual problem description in
#' #the messageGenerator-call:
#'  identifyUnderscores2 <- function(v, nMax = Inf) {
#'   v <- as.character(v)
#'   underscorePlaces <- regexpr("_", v) > 0
#'   problemValues <- unique(v[underscorePlaces])
#'   problem <- any(underscorePlaces)
#'   message <- messageGenerator(list(problemValues = problemValues,
#'                                    problem = problem), nMax = nMax)
#'   checkResult(list(problem = problem, message = message,
#'       problemValues = problemValues))
#'  }
#'
#'  identifyUnderscores2(noUSVar) #no problem
#'  identifyUnderscores2(USVar) #problems
#'
#' @include checkResult.R
#' @export
messageGenerator <- function(problemStatus, 
                             message = "Note that a check function found the following problematic values:", 
                             nMax = 10) {

#   ##what functions made the call the message generator?
#    ## This should be removed
#  callF <- sys.status()$sys.calls
#  callF <- sapply(callF, function(x) as.character(x[1]))
#
#  #standard functions do not need to specify a message when called
#  standardCall <- intersect(callF, c("identifyMissing", "identifyWhitespace", "identifyOutliers",
#                                     "identifyLoners", "identifyCaseIssues", "identifyOutliersTBStyle"))
#
#  if (length(standardCall==1)) {
#    messages <- list(identifyMissing =
#                       "The following suspected missing value codes enter as regular values:",
#                     identifyWhitespace =
#                       "The following values appear with prefixed or suffixed white space:",
#                     identifyOutliers =
#                       "Note that the following possible outlier values were detected:",
#                     identifyLoners =
#                       "Note that the following levels have at most five observations:",
#                     identifyCaseIssues =
#                       "Note that there might be case problems with the following levels:",
#                     identifyOutliersTBStyle =
#                       "Note that the following possible outlier values were detected:")
#    check <- standardCall
#  } else {
#    callF <- callF[length(callF) - 2]
#    if (is.null(message)) {
#      message <- paste(callF, "found the following problem values:")
#    }
#    messages <- list(onlyMessage = message)
#    check <- "onlyMessage"
#  }
#
#  ifelse(problemStatus$problem,
#         paste(paste(messages[[check]], printProblemValues(problemStatus$problemValues, nMax)),
#               ".", sep = ""),
#         "")
  
  ifelse(problemStatus$problem,
         paste(paste(message, printProblemValues(problemStatus$problemValues, nMax)),
               ".", sep = ""),
         "")
  
}



#############################Not exported below##################################################

#Formats problems values, i.e. escapes characters appropriately for rmarkdown
#and adds quotes such that prefixed and suffixed blankspaces are visible.
#only called when there is at least one problemValue
printProblemValues <- function(problemValues, nMax = Inf) {
    problemValues <- escapeRMDStyle(sort(problemValues, na.last = TRUE))
        ##NOTE: sort removes NaNs if not told explicitly not to by use of the na.last-argument
    nVals <- length(problemValues)
    extraStr <- ""
    if (nMax < nVals) {
      if (nMax == 0) {
        problemValues <- ""
        extraStr <- paste(nVals-nMax, "problematic value(s) omitted")
      } else {
        problemValues <- problemValues[1:nMax]
        extraStr <- paste(" (", nVals-nMax, " additional values omitted)", sep="")
      }
    }
    paste(paste(paste("\\\"", problemValues, "\\\"", sep=""),
                collapse=", "), extraStr, sep="")
}

#Does character escaping such that the text can be rendered by rmarkdown
#without any issues. 
#The following types of issues are addressed so far: html tags, \s
#
#NOTE: 3 slashes escapes the espaced string [\"] such that it is printed correctly
#(and not interpreted) in markdown.
escapeRMDStyle <- function(string) {
  string <- gsub("\n", "\\n", string, fixed = TRUE)
  string <- gsub("\\", "\\\\", string, fixed = TRUE)
 # string <- gsub("\a", "\\\\\\\\a", string)
#  string <- gsub("\f", "\\\\\\\\f", string)
#  string <- gsub("\n", "\\\\\\\\n", string)
#  string <- gsub("\r", "\\\\\\\\r", string)
#  string <- gsub("\t", "\\\\\\\\t", string)
#  string <- gsub("<", "\\<", string, fixed = TRUE)
#  gsub(">", "\\>", string, fixed = TRUE)
  string
}


#Helper function for escaping characters such that they are 
#printed as is.
#All currently implemented special characters commencing 
#with "\" are included (as of 02-08-2017)
escapeRStyle <- function(string) {
  string <- gsub("\\", "\\\\", string, fixed = TRUE)
  string <- gsub("\a", "\\\\a", string)
  string <- gsub("\f", "\\\\f", string)
  string <- gsub("\n", "\\\\n", string)
  string <- gsub("\r", "\\\\r", string)
  string <- gsub("\t", "\\\\t", string)
  string
}

#' @title A checkFunction for identifying case issues
#'
#' @description A \code{\link{checkFunction}} to be called from
#' \code{\link{check}} that identifies values in a vector
#' that appear multiple times with different case settings.
#'
#' @param v A character, factor, haven_labelled or labelled variable to check.
#'
#' @param nMax The maximum number of problematic values to report. 
#' Default is \code{10}. Set to \code{Inf} if all problematic values are to be included 
#' in the outputted message, or to \code{0} for no output.
#'
#' @return A \code{\link{checkResult}} with three entires:
#' \code{$problem} (a logical indicating whether case issues where found),
#' \code{$message} (a message describing which values in \code{v} resulted
#' in case issues) and \code{$problemValues} (the problematic values
#' in their original format). Note that Only unique problematic values
#' are listed and they are presented in alphabetical order.
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}},
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#'
#' @examples
#'  identifyCaseIssues(c("val", "b", "1", "1", "vAl", "VAL", "oh", "OH"))
#'
#' @importFrom stats na.omit
#' @export
identifyCaseIssues <- function(v, nMax = 10) UseMethod("identifyCaseIssues")


#add methods to generic identifyCaseIssues function
#' @export
identifyCaseIssues.character <- function(v, nMax = 10) identifyCaseIssuesC(v, nMax = nMax)

#' @export
identifyCaseIssues.factor <- function(v, nMax = 10) identifyCaseIssuesF(v, nMax = nMax)

#' @export
identifyCaseIssues.labelled <- function(v, nMax = 10) identifyCaseIssuesL(v, nMax = nMax)

#' @export
identifyCaseIssues.haven_labelled <- function(v, nMax = 10) identifyCaseIssuesL(v, nMax = nMax)

#make it a checkFunction
#' @include checkFunction.R
identifyCaseIssues <- checkFunction(identifyCaseIssues, "Identify case issues",
                                    c("character", "factor"))


##########################################Not exported below#########################################

identifyCaseIssuesMessage <- "Note that there might be case problems with the following levels:"

#character variable
identifyCaseIssuesC <- function(v, nMax) {
  v <- na.omit(v)
  vLevs <- unique(v)
  vLevsLower <- tolower(vLevs)
  problemOcc <- vLevs[which(duplicated(vLevsLower) | duplicated(vLevsLower, fromLast = TRUE))]
  if (length(problemOcc) > 0) {
    problem <- TRUE
    problemValues <- sort(problemOcc)
  } else {
    problem <- FALSE
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem = problem,
                                      problemValues = problemValues),
                                 message = identifyCaseIssuesMessage,
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage, problemValues = problemValues))
}


#factor variable
identifyCaseIssuesF <- function(v, nMax) {
  v <- as.character(v)
  identifyCaseIssuesC(v, nMax = nMax)
}

#labelled variable
identifyCaseIssuesL <- function(v, nMax) {
  identifyCaseIssuesF(dataMaid_as_factor(v), nMax)
}

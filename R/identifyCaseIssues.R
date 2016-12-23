#' @title A checkFunction for identifying case issues
#'
#' @description A \code{\link{checkFunction}} to be called from
#' \code{\link{check}} that identifies values in a vector
#' that appear multiple times with different case settings.
#'
#' @param v A character, factor, or labelled variable to check.
#'
#' @param nMax The maximum number of problematic values to report.
#' Default is \code{Inf}, in which case all problematic values
#' are included in the outputted message.
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
identifyCaseIssues <- function(v, nMax = Inf) UseMethod("identifyCaseIssues")


#add methods to generic identifyCaseIssues function
#' @export
identifyCaseIssues.character <- function(v, nMax = Inf) identifyCaseIssuesC(v, nMax = nMax)

#' @export
identifyCaseIssues.factor <- function(v, nMax = Inf) identifyCaseIssuesF(v, nMax = nMax)

#' @export
identifyCaseIssues.labelled <- function(v, nMax = Inf) identifyCaseIssuesF(v, nMax = nMax)


#make it a checkFunction
#' @include checkFunction.R
identifyCaseIssues <- checkFunction(identifyCaseIssues, "Identify case issues",
                                    c("character", "factor"))


##########################################Not exported below#########################################

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
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage, problemValues = problemValues))
}


#factor variable
identifyCaseIssuesF <- function(v, nMax) {
  v <- as.character(v)
  identifyCaseIssuesC(v, nMax = nMax)
}



#' @title A checkFunction for identifying case issues
#' 
#' @description A checkFunction to be called from \code{\link{check}} that identifies values in a vector
#' that appear multiple times with different case settings.
#' 
#' @param v A character or factor variable to check
#' 
#' @return A list with two elements, $problem: TRUE if any case issues were found, FALSE otherwise, and
#' $message A message describing which values in \code{v} resulted in case issues. Only unique values
#' are listed and they are presented in alphabetical order.
#' 
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
#' 
#' @examples
#'  identifyCaseIssues(c("val", "b", "1", "1", "vAl", "VAL", "oh", "OH"))
#' 
#' @importFrom stats na.omit
#' @export
identifyCaseIssues <- function(v) UseMethod("identifyCaseIssues")
identifyCaseIssues <- checkFunction(identifyCaseIssues, "Identify case issues")





##########################################Not exported below#########################################

#character variable
identifyCaseIssuesC <- function(v) {
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
                                 "identifyCaseIssues")
  list(problem = problem, message = outMessage)
}


#factor variable
identifyCaseIssuesF <- function(v) {
  v <- as.character(v)
  identifyCaseIssuesC(v)
}


#add methods to generic identifyCaseIssues function
identifyCaseIssues.character <- function(v) identifyCaseIssuesC(v)
identifyCaseIssues.factor <- function(v) identifyCaseIssuesF(v)


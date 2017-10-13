#' A checkFunction for identifying sparsely represented values (loners)
#'
#' A \code{\link{checkFunction}} to be called from \code{\link{check}} that identifies values that
#' only occur less than 6 times in factor, labelled, or character variables (that is, loners).
#'
#' @param v A character, labelled, or factor variable to check.
#'
#' @param nMax The maximum number of problematic values to report. 
#' Default is \code{10}. Set to \code{Inf} if all problematic values are to be included 
#' in the outputted message, or to \code{0} for no output.
#'
#' @return A \code{\link{checkResult}} with three entires:
#' \code{$problem} (a logical indicating whether case issues where found),
#' \code{$message} (a message describing which values in \code{v} were loners) and
#' \code{$problemValues} (the problematic values in their original format).
#' Note that Only unique problematic values
#' are listed and they are presented in alphabetical order.
#'
#' @details For character, labelled, and factor variables, identify values that only have a
#' very low number of observations, as these categories might be
#' problematic when conducting an analysis. Unused factor levels are
#' not considered "loners". "Loners" are defined as values with 5 or less
#' observations, reflecting the commonly use rule of thumb for performing
#' chi squared tests.
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}},
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#'
#' @examples
#' identifyLoners(c(rep(c("a", "b", "c"), 10), "d", "d"))
#'
#' @importFrom stats na.omit
#' @export
identifyLoners <- function(v, nMax = 10) UseMethod("identifyLoners")


#add methods to generic identifyLoners function
#' @export
identifyLoners.factor <- function(v, nMax = 10) identifyLonersF(v, nMax = nMax)
#' @export
identifyLoners.labelled <- function(v, nMax = 10) identifyLonersL(v, nMax = nMax)
#' @export
identifyLoners.character <- function(v, nMax = 10) identifyLonersC(v, nMax = nMax)


#make it a checkFunction
#' @include checkFunction.R
identifyLoners <- checkFunction(identifyLoners, "Identify levels with < 6 obs.",
                                c("character", "factor"))

##########################################Not exported below#########################################

identifyLonersMessage <- "Note that the following levels have at most five observations:"

#For character/factor variables, identify values that only have a
#very low number of observations, as these categories might be
#problematic when conducting an analysis. Unused factor levels are
#not considered "loners". "Loners" have 5 or less observations.


#factor variables
identifyLonersF <- function(v, nMax) {
  vLev <- levels(v)
  v <- factor(na.omit(v)) #drop unused levels
  lonerOcc <- vLev[which(table(v) <= 5)]
  if (length(lonerOcc) > 0) {
    problem <- TRUE
    problemValues <- lonerOcc
  } else {
    problem <- FALSE
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 message = identifyLonersMessage,
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage,
                   problemValues = problemValues))
}

#character variables
identifyLonersC <- function(v, nMax) {
  v <- factor(v)
  identifyLonersF(v, nMax)
}

identifyLonersL <- function(v, nMax) {
  v <- haven::as_factor(v)
  identifyLonersF(v, nMax)
}


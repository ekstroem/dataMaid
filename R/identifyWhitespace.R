#' @title A checkFunction for identifying whitespace
#'
#' @description A checkFunction to be called from \code{\link{check}}
#' that identifies prefixed and suffixed whitespace(s) in character,
#' (haven_)labelled or factor variables.
#'
#' @param v A character, (haven_)labelled or factor variable to check.
#'
#' @param nMax The maximum number of problematic values to report. 
#' Default is \code{10}. Set to \code{Inf} if all problematic values are to be included 
#' in the outputted message, or to \code{0} for no output.
#'
#' @return  A \code{\link{checkResult}} with three entires:
#' \code{$problem} (a logical indicating whether any whitespaces were
#' fount), \code{$message} (a message describing which values were prefixed
#' or suffixed with whitespace) and \code{$problemValues} (the problematic
#' values). Note that only unique values are printed in the message, and that
#' they are sorted alphabetically.
#'
#' @seealso \code{\link{check}}, \code{\link{allCheckFunctions}},
#' \code{\link{checkFunction}}, \code{\link{checkResult}}
#'
#' @examples
#'  identifyWhitespace(c("a", " b", "c", "d ", "e  "))
#'
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @export
identifyWhitespace <- function(v, nMax = 10) UseMethod("identifyWhitespace")


#add methods to generic identifyWhitespace function

#'@export
identifyWhitespace.character <- function(v, nMax = 10) identifyWhitespaceC(v, nMax = nMax)

#'@export
identifyWhitespace.factor <- function(v, nMax = 10) identifyWhitespaceF(v, nMax = nMax)

#'@export
identifyWhitespace.labelled <- function(v, nMax = 10) identifyWhitespaceL(v, nMax = nMax)

#'@export
identifyWhitespace.haven_labelled <- function(v, nMax = 10) identifyWhitespaceL(v, nMax = nMax)

#make it a checkFunction
#' @include checkFunction.R
identifyWhitespace <- checkFunction(identifyWhitespace, "Identify prefixed and suffixed whitespace",
                                    c("character", "factor", "labelled", "haven_labelled"))


##########################################Not exported below#########################################
identifyWhitespaceMessage <- "The following values appear with prefixed or suffixed white space:"

#character variables
identifyWhitespaceC <- function(v, nMax) {
  v <- na.omit(v)
  # wsPrefixPlaces <- sapply(v, substr, 1, 1) == " "
  wsPrefixPlaces <- substr(v, 1, 1) == " "
  # wsSuffixPlaces <- sapply(v, function(x) {tail(strsplit(x, "")[[1]], 1)}) == " "
  wsSuffixPlaces <- substr(v, nchar(v), nchar(v)) == " "

  allWsPlaces <- wsPrefixPlaces | wsSuffixPlaces
  if (any(allWsPlaces)) {
    problem <- TRUE
    problemValues <- unique(v[allWsPlaces])
  } else {
    problem <- FALSE
    problemValues <- NULL
  }
  outMessage <- messageGenerator(list(problem=problem,
                                      problemValues=problemValues),
                                 message = identifyWhitespaceMessage,
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage,
                   problemValues = problemValues))
}

#factor variables
identifyWhitespaceF <- function(v, nMax) {
  identifyWhitespaceC(as.character(v), nMax)
}

#labelled variables
identifyWhitespaceL <- function(v, nMax) {
  v <- na.omit(dataMaid_as_factor(v))
  identifyWhitespaceF(v, nMax = nMax)
}




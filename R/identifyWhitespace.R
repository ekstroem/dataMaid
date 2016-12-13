#' @title A checkFunction for identifying whitespace
#'
#' @description A checkFunction to be called from \code{\link{check}} that identifies prefixed and
#' suffixed whitespace(s) in character, labelled or factor variables.
#'
#' @param v A character, labelled or factor variable to check.
#' @param nMax The maximum number of problematic values to report. Default is \code{Inf}, in which case
#' all problematic values are included in the outputtet message.
#'
#' @return A list with two elements, $problem: TRUE if any whitespaces were found, FALSE otherwise, and
#' $message A message describing which values in \code{v} were prefixed or suffixed with whitespace.
#' Note that only unique values are printed and that they are sorted alphabetically.
#'
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
#'
#' @examples
#'  identifyWhitespace(c("a", " b", "c", "d ", "e  "))
#'
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @export
identifyWhitespace <- function(v, nMax = Inf) UseMethod("identifyWhitespace")


#add methods to generic identifyWhitespace function

#'@export
identifyWhitespace.character <- function(v, nMax = Inf) identifyWhitespaceC(v, nMax = nMax)

#'@export
identifyWhitespace.factor <- function(v, nMax = Inf) identifyWhitespaceF(v, nMax = nMax)

#'@export
identifyWhitespace.labelled <- function(v, nMax = Inf) identifyWhitespaceL(v, nMax = nMax)


#make it a checkFunction
identifyWhitespace <- checkFunction(identifyWhitespace, "Identify prefixed and suffixed whitespace")


##########################################Not exported below#########################################


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
                                 nMax = nMax)
  checkResult(list(problem = problem, message = outMessage, problemValues = problemValues))
}

#factor variables
identifyWhitespaceF <- function(v, nMax) {
  identifyWhitespaceC(as.character(v), nMax)
}

#labelled variables
identifyWhitespaceL <- function(v, nMax) {
  v <- na.omit(v)
  v <- unpackLabelled(v)
  identifyWhitespaceC(v, nMax = nMax)
}




#' @title A checkFunction for identifying whitespace
#'
#' @description A checkFunction to be called from \code{\link{check}} that identifies prefixed and
#' suffixed whitespace(s) in character, labelled or factor variables.
#'
#' @param v A character, labelled or factor variable to check.
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
identifyWhitespace <- function(v) UseMethod("identifyWhitespace")
identifyWhitespace <- checkFunction(identifyWhitespace, "Identify prefixed and suffixed whitespace")


#add methods to generic identifyWhitespace function

#'@export
identifyWhitespace.character <- function(v) identifyWhitespaceC(v)

#'@export
identifyWhitespace.factor <- function(v) identifyWhitespaceF(v)

#'@export
identifyWhitespace.labelled <- function(v) identifyWhitespaceL(v)


##########################################Not exported below#########################################


#character variables
identifyWhitespaceC <- function(v) {
  v <- na.omit(v)
  wsPrefixPlaces <- sapply(v, substr, 1, 1) == " "
  wsSuffixPlaces <- sapply(v, function(x) {tail(strsplit(x, "")[[1]], 1)}) == " "
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
                                 "identifyWhitespace")
  list(problem=problem, message=outMessage)
}

#factor variables
identifyWhitespaceF <- function(v) {
  identifyWhitespaceC(as.character(v))
}

#labelled variables
identifyWhitespaceL <- function(v) {
  v <- na.omit(v)
  v <- unpackLabelled(v)
  identifyWhiteSpaceC(v)
}




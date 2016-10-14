#' @title A checkFunction for identifying miscoded missing values.
#'
#' @description A checkFunction to be called from \code{\link{check}} that identifies values that
#' appear to be miscoded missing values.
#'
#' @param v A variable to check.
#'
#' @details \code{identifyMissing} tries to identify common choices of missing values outside of the
#' R standards. These include special words (NaN and Inf (no matter the cases)), one or more 9's
#' (e.g. 999 or "99"), STATA style missing values (commencing with ".") and other character strings
#' ("", " ", "-", "NA" miscoded as character).
#'
#' @return A list with two elements, $problem: TRUE if any miscoded missing values were found,
#' FALSE otherwise, and $message A message describing which values in \code{v} were miscoded missing
#' values. Note that only unique values are listed and that they appear in alphabetical order.
#'
#' @seealso \code{\link{check}}, \code{\link[clean]{checkFunction}}
#'
#' @examples
#'  data(testData)
#'  testData$miscodedMissingVar
#'  identifyMissing(testData$miscodedMissingVar)
#'
#' @importFrom stats na.omit
#' @export
identifyMissing <- function(v) UseMethod("identifyMissing")
identifyMissing <- checkFunction(identifyMissing, "Identify miscoded missing values")



##########################################Not exported below#########################################



#NOTE: I fill out missStrs manually to avoid having to run the same
#bit of code each time identifyMiss is called. Better way to do it?

#Deal with 9, 99, 999, ... by converting to characterstring. Not
#very elegant, find better way.
#Takes a vector and returns the values that consist
#of more than one character/digit and consist only of 9's, e.g.
#99, 999, 999999 (but NOT 9).
#update: also works for other characters than 9, then it finds all
#occurances that consist of only that character, excluding a single
#occurance, e.g. just "9" or "" or "a", but identifying "99", "   ", "aaa"
identifyMissRepChar <- function(v, char) {
  v <- as.character(v)
  char <- as.character(char)
  v[v==char] <- "char"
  v[v==""] <- "empty"
  problems <- unlist(lapply(lapply(strsplit(v, ""), "==", char), all))
  if (any(problems)) {
    vals <- unique(v[problems])
  } else vals <- NULL
  vals
}


#Should only be used on numeric/integer vectors.
#returns 9 if 9 seems to act as a missing value indicator
#in the variable
identifyMissNine <- function(v) {
  nines <- v == 9
  if (!any(nines)) return(NULL)
  if (max(v)>9) return(NULL)
  if (length(unique(v))>8) return(NULL)
  9
}

#factor and character variables
identifyMissingCF <- function(v) {
  v <- na.omit(v)
  problem <- FALSE
  problemValues <- NULL

  missStrs <- c("", "nan", "NaN", "NAN", "na", "NA", "Na", "Inf", "inf",
                "-Inf", "-inf", "-", " ", "9")
  missStrsOcc <- intersect(v, missStrs) #what potential missing value strings occur?

  missNinesOcc <- identifyMissRepChar(v, "9") #what 99, 999, ... strings occur?
  missSpaceOcc <- identifyMissRepChar(v, " ") #what "  ", "   ", ... strings occur?

  #STATA-style: .something describes a "something" type of missing value
  missDotPrefixOcc <- unique(v[substr(v, 1, 1) == "."])

  allProblemOcc <- c(missStrsOcc, missNinesOcc, missSpaceOcc, missDotPrefixOcc)

  if (length(allProblemOcc) > 0) {
    problemValues <- allProblemOcc
    problem <- TRUE
  }
  outMessage <- messageGenerator(list(problem = problem, problemValues = problemValues),
                                 check = "identifyMiss")
  list(problem = problem, message = outMessage)
}

#labbeled variables
identifyMissingL <- function(v) {
  v <- na.omit(v)
  v <- unpackLabelled(v)
  identifyMissingCF(v)
}

#numerical and integer variables
identifyMissingNI <- function(v) {
  v <- na.omit(v)
  problem <- FALSE
  problemValues <- NULL
  finiteInd <- is.finite(v)

  missNinesOcc <- identifyMissRepChar(max(v[finiteInd]), "9") #what 99, 999, ... values occur?
  missNaNOcc <- unique(v[!finiteInd]) #what NaN, Inf, ... values occur?
  missNineOcc <- identifyMissNine(v) #returns 9 if 9 seems to act as missing value
  allProblemOcc <- c(missNinesOcc, missNaNOcc, missNineOcc)

  if (length(allProblemOcc) > 0) {
    problemValues <- allProblemOcc
    problem <- TRUE
  }
  outMessage <- messageGenerator(list(problem = problem, problemValues = problemValues),
                                 check = "identifyMiss")
  list(problem = problem, message = outMessage)
}

#logical (B = boolean) variables
#Note: for v to have class logical, only T, F and NA can occur, thus there are
#no potentially miscoded missing values.
#Note: this function catches variables that only consist of NA's, thus it is
#safe to omit NAs in the other functions, without risking obtaining an empty vector
identifyMissingB <- function(v) {
  list(problem = FALSE, message = "")
}

#Add methods to generic identifyMiss
#' @export
identifyMissing.character <- function(v) identifyMissingCF(v)
#' @export
identifyMissing.factor <- function(v) identifyMissingCF(v)
#' @export
identifyMissing.labelled <- function(v) identifyMissingL(v)
#' @export
identifyMissing.numeric <- function(v) identifyMissingNI(v)
#' @export
identifyMissing.integer <- function(v) identifyMissingNI(v)
#' @export
identifyMissing.logical <- function(v) identifyMissingB(v)


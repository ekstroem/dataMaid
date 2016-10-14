#' A checkFunction for identifying sparsely represented values (loners)
#'
#' A checkFunction to be called from \code{\link{check}} that identifies values that
#' only occur less than 6 times in factor or character variables (that is, loners).
#'
#' @param v A character or factor variable to check.
#'
#' @return A list with two elements, $problem: TRUE if any loners were found, FALSE otherwise, and
#' $message A message describing which values in \code{v} were loners. Note that loner values
#' are listed only once (no matter how many time they occur) in alphabetical order.
#'
#' @seealso \code{\link{check}}, \code{\link{checkFunction}}
#'
#' @examples
#' identifyLoners( c(rep(c("a", "b", "c"), 10), "d", "d") )
#'
#' @importFrom stats na.omit
#' @export
identifyLoners <- function(v) UseMethod("identifyLoners")
identifyLoners <- checkFunction(identifyLoners, "Identify levels with < 6 obs.")


#add methods to generic identifyLoners function
#' @export
identifyLoners.factor <- function(v) identifyLonersF(v)
#' @export
identifyLoners.character <- function(v) identifyLonersC(v)

##########################################Not exported below#########################################


#For character/factor variables, identify values that only have a
#very low number of observations, as these categories might be
#problematic when conducting an analysis. Unused factor levels are
#not considered "loners". "Loners" have 5 or less observations.
#(corresponding to the chi2-test rule of thumb)
#NOTE: Different (or just more) loner definition(s)?
#NOTE: Proper term for "loner"?
#NOTE: identifyLoner for integer/numerical variables suspected to be categorical?
#NOTE: identifyLoner for labelled variables?


#factor variables
identifyLonersF <- function(v) {
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
                                 "identifyLoners")
  list(problem=problem, message=outMessage)
}

#character variables
identifyLonersC <- function(v) {
  v <- factor(v)
  identifyLonersF(v)
}



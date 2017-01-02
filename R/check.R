#General comments:
# arguments: v is a variable (column) from a data.frame, i.e. a vector
# note: labelled is Wickham's class from the haven/hmisc packages
# note: Function names are suffixed with an indicator of the data types it
#       is to be called on. N is numerical, I is integer, F is factor,
#       L is labelled (Wickham), C is character, D is Date, B is logical (boolean)
#       The ordering of suffix letters is arbitrary.
#       All suffixed functions are purely internal
#       and their calling is controlled using s3 methods


####################################################################################
###################################Check############################################
####################################################################################

#' Perform a check of potential errors in a data frame
#'
#' Run a set of validation checks to check a variable vector or a full dataset
#' for potential errors.
#' Which checks are performed depends on the class of the variable and on user
#' inputs.
#'
#' @param v the vector or the dataset (\code{data.frame}) to be checked.
#' @param nMax If a check is supposed to identify problematic values,
#' this argument controls if all of these should be pasted onto the outputted
#' message, or if only the first \code{nMax} should be included. The default
#' (\code{Inf}) prints all problematic values.
#' @param \dots Other arguments that are passed on to the checking functions.
#' These includes general parameters controlling how the check results are
#' formatted (e.g. \code{maxDecimals}, which controls the number of decimals
#' printed for numerical, problematic values), and also parameters that
#' control what check functions are called for each variable type. The latter
#' arguments all follow the template \code{[class]Checks}, see examples below
#' for more details.
#'
#' @return If \code{v} is a variable, a list of objects of class
#' \code{\link{checkResult}}, which each summarizes the result of a
#' \code{\link{checkFunction}} call performed on \code{v}.
#' See \code{\link{checkResult}} for more details. If \code{V} is a
#' \code{data.frame}, a list of lists of the form above
#' is returned instead with one entry for each variable in \code{v}.
#'
#' @details It should be noted that the default options for each variable type
#' are returned by calling e.g. \code{defaultCharacterChecks()},
#' \code{defaultFactorChecks()}, \code{defaultNumericChecks()}, etc. Moreover,
#' all available \code{checkFunction}s (including both locally defined
#' functions and functions imported from \code{dataMaid} or other packages) can
#' be viewed by calling \code{allCheckFunctions()}.
#'
#'
#' @seealso \code{\link{allCheckFunctions}} \code{\link{checkResult}}
#' \code{\link{checkFunction}}, \code{\link{defaultCharacterChecks}},
#' \code{\link{defaultFactorChecks}}, \code{\link{defaultLabelledChecks}},
#' \code{\link{defaultNumericChecks}}, \code{\link{defaultIntegerChecks}},
#' \code{\link{defaultLogicalChecks}}, \code{\link{defaultDateChecks}}
#' @keywords misc
#' @examples
#'
#' x <- 1:5
#' check(x)
#'
#' #Annoyingly coded missing as 99
#' y <- c(rnorm(100), rep(99, 10))
#' check(y)
#'
#' #Change what checks are performed on a variable, now only identifyMissing is called
#' # for numeric variables
#' check(y, numericChecks = "identifyMissing")
#'
#' #Check a full data.frame at once
#' data(cars)
#' check(cars)
#'
#' #Check a full data.frame at once, while changing the standard settings for
#' #several data classes at once and including all decimals in problematic
#' #values.
#' #Here, we ommit the check of miscoded missing values for factors
#' #and we only do this check for numeric variables.
#' check(cars, factorChecks = setdiff(defaultFactorChecks(), "identifyMissing"),
#'   numericChecks = "identifyMissing")
#'
#' @export
check <- function(v, nMax = Inf, ...) UseMethod("check")


#methods and default options for each variable class

#' @title Default checks for character variables
#'
#' @description Default options for which checks to perform on
#' character type variables in \code{\link{check}} and \code{\link{clean}}.
#'
#' @return A vector of function names.
#'
#' @export
defaultCharacterChecks <- function() c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                       "identifyCaseIssues", "identifyNums")
#' @export
check.character <- function(v, nMax =  Inf, characterChecks=defaultCharacterChecks(), ...) {
  out <- lapply(characterChecks, function(x) eval(call(x, v = v, nMax = nMax)))
  names(out) <- characterChecks
  out
}


#' @title Default checks for factor variables
#'
#' @description Default options for which checks to perform on
#' factor type variables in \code{\link{check}} and \code{\link{clean}}.
#'
#' @return A vector of function names.
#'
#' @export
defaultFactorChecks <- function() c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                       "identifyCaseIssues", "identifyNums")
#' @export
check.factor <- function(v, nMax =  Inf, factorChecks = defaultFactorChecks(), ...) {
  out <- lapply(factorChecks, function(x) eval(call(x, v = v, nMax = nMax)))
  names(out) <- factorChecks
  out
}


#' @title Default checks for labelled variables
#'
#' @description Default options for which checks to perform on
#' labelled type variables in \code{\link{check}} and \code{\link{clean}}.
#'
#' @return A vector of function names.
#'
#' @export
defaultLabelledChecks <- function() c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                      "identifyCaseIssues", "identifyNums")


#' @export
check.labelled <- function(v, nMax =  Inf, labelledChecks = defaultLabelledChecks(), ...) {
  out <- lapply(labelledChecks, function(x) eval(call(x, v = v, nMax = nMax)))
  names(out) <- labelledChecks
  out
}


#' @title Default checks for numeric variables
#'
#' @description Default options for which checks to perform on
#' numeric type variables in \code{\link{check}} and \code{\link{clean}}.
#'
#' @return A vector of function names.
#'
#' @export
defaultNumericChecks <- function() c("identifyMissing", "identifyOutliers")

#' @export
check.numeric <- function(v, nMax =  Inf, maxDecimals = 2,
                          numericChecks = defaultNumericChecks(), ...) {
  out <- lapply(numericChecks, function(x) eval(call(x, v = v, nMax = nMax,
                                              maxDecimals = maxDecimals)))
  names(out) <- numericChecks
  out
}


#' @title Default checks for integer variables
#'
#' @description Default options for which checks to perform on
#' integer type variables in \code{\link{check}} and \code{\link{clean}}.
#'
#' @return A vector of function names.
#'
#' @export
defaultIntegerChecks <- function() c("identifyMissing", "identifyOutliers")

#' @export
check.integer <- function(v, nMax =  Inf, maxDecimals = 2,
                          integerChecks = defaultIntegerChecks(), ...) {
  out <- lapply(integerChecks, function(x) eval(call(x, v = v, nMax = nMax,
                                              maxDecimals = maxDecimals)))
  names(out) <- integerChecks
  out
}



#' @title Default checks for logical variables
#'
#' @description Default options for which checks to perform on
#' logical type variables in \code{\link{check}} and \code{\link{clean}}.
#'
#' @return A vector of function names.
#'
#' @export
defaultLogicalChecks <- function() NULL #NOTE: we don't actually do any logical checks...

#' @export
check.logical <- function(v, nMax =  Inf, logicalChecks = defaultLogicalChecks(), ...) {
  if (! is.null(logicalChecks)) {
    out <- lapply(logicalChecks, function(x) eval(call(x, v=v, nMax = nMax)))
    names(out) <- logicalChecks
    return(out)
  } else return(list(NoChecksPerformed = checkResult(list(problem = FALSE,
                                                        message = "",
                                                        problemValues = NULL))))
}


#' @title Default checks for Date variables
#'
#' @description Default options for which checks to perform on
#' Date type variables in \code{\link{check}} and \code{\link{clean}}.
#'
#' @return A vector of function names.
#'
#' @export
defaultDateChecks <- function() "identifyOutliers"

#' @export
check.Date <- function(v, nMax =  Inf, DateChecks = defaultDateChecks(), ...) {
  if (! is.null(DateChecks)) {
    out <- lapply(DateChecks, function(x) eval(call(x, v=v, nMax = nMax)))
    names(out) <- DateChecks
    return(out)
  } else return(list(NoChecksPerformed = checkResult(list(problem = FALSE,
                                                        message = "",
                                                        problemValues = NULL))))
}



#' @export
check.data.frame <- function(v, nMax = Inf, maxDecimals = 2, ...) {
  lapply(v, check, nMax = nMax, maxDecimals = maxDecimals, ...)
}



##########################################Not exported below#########################################

#empty

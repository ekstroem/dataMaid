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

#' Perform checks of potential errors in variable/dataset
#'
#' Run a set of validation checks to check a variable vector or a full dataset
#' for potential errors.
#' Which checks are performed depends on the class of the variable and on user
#' inputs.
#'
#' @param v the vector or the dataset (\code{data.frame}) to be checked.
#' @param nMax If a check is supposed to identify problematic values,
#' this argument controls if all of these should be pasted onto the outputted
#' message, or if only the first \code{nMax} should be included. If set to \code{Inf}, 
#' all problematic values are printed.
#' @param checks A list of checks to use on each supported variable type. We recommend
#' using \code{\link{setChecks}} for creating this list and refer to the documentation
#' of this function for more details.
#' @param \dots Other arguments that are passed on to the checking functions.
#' These includes general parameters controlling how the check results are
#' formatted (e.g. \code{maxDecimals}, which controls the number of decimals
#' printed for numerical, problematic values).
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
#' \code{defaultFactorChecks()}, \code{defaultNumericChecks()}, etc. A complete 
#' overview of all default options can be obtained by calling \code{setChecks()}. 
#' Moreover, all available \code{checkFunction}s (including both locally defined
#' functions and functions imported from \code{dataMaid} or other packages) can
#' be viewed by calling \code{allCheckFunctions()}.
#'
#'
#' @seealso \code{\link{setChecks}}, 
#' \code{\link{allCheckFunctions}} \code{\link{checkResult}}
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
#' #Check y for outliers and print 4 decimals for problematic variables
#' check(y, checks = setChecks(numeric = "identifyOutliers"), maxDecimals = 4)
#'
#' #Change what checks are performed on a variable, now only identifyMissing is called
#' # for numeric variables
#' check(y, checks = setChecks(numeric = "identifyMissing"))
#'
#' #Check a full data.frame at once
#' data(cars)
#' check(cars)
#'
#' #Check a full data.frame at once, while changing the standard settings for
#' #several data classes at once. Here, we ommit the check of miscoded missing values for factors
#' #and we only do this check for numeric variables:
#' check(cars, checks = setChecks(factor = defaultFactorChecks(remove = "identifyMissing"),
#'   numeric = "identifyMissing"))
#'
#' @export
check <- function(v, nMax = 10, checks = setChecks(), ...) UseMethod("check")


#methods and default options for each variable class

#Catch non-supported classes, do nothing and throw a warning:
#' @export
check.default <- function(v, nMax = 10, checks = setChecks(), ...) {
  vClass <- class(v)[1]
  warning(paste("A variable of class", vClass, 
                "was supplied. This is not supported by dataMaid.",
                "No checks were performed."))
  list(NoChecksPerformed = checkResult(list(problem = FALSE,
                                            message = "",
                                            problemValues = NULL)))
  
}


#' @title Default checks for character variables
#' 
#' @param remove Character vector of function names. Checks to remove from the returned vector 
#' 
#' @param add Character vector of function names. Checks to add to the returned vector
#'
#' @description Default options for which checks to perform on
#' character type variables in \code{\link{check}} and \code{\link{makeDataReport}},
#' possibly user-modified by adding extra function names using \code{add} or 
#' removing default function names with \code{remove}. 
#'
#' @return A vector of function names.
#'
#' @export
defaultCharacterChecks <- function(remove = NULL, add = NULL) {
  defVals <-  c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                       "identifyCaseIssues", "identifyNums")
  unique(c(setdiff(defVals, remove), add))
}

#' @export
check.character <- function(v, nMax =  10, checks = setChecks(),
                            characterChecks = NULL, 
                             ...) {
  if (is.null(characterChecks)) characterChecks <- checks$character
  out <- lapply(characterChecks, function(x) eval(call(x, v = v, nMax = nMax)))
  names(out) <- characterChecks
  out
}


#' @title Default checks for factor variables
#'
#' @param remove Character vector of function names. Checks to remove from the returned vector 
#' 
#' @param add Character vector of function names. Checks to add to the returned vector
#'
#' @description Default options for which checks to perform on
#' factor type variables in \code{\link{check}} and \code{\link{makeDataReport}},
#' possibly user-modified by adding extra function names using \code{add} or 
#' removing default function names with \code{remove}. 
#'
#' @return A vector of function names.
#'
#' @export
defaultFactorChecks <- function(remove = NULL, add = NULL) {
  defVals <- c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                       "identifyCaseIssues", "identifyNums")
  unique(c(setdiff(defVals, remove), add))
}

#' @export
check.factor <- function(v, nMax =  10, checks = setChecks(),
                         factorChecks = NULL, ...) {
  if (is.null(factorChecks)) factorChecks <- checks$factor
  out <- lapply(factorChecks, function(x) eval(call(x, v = v, nMax = nMax)))
  names(out) <- factorChecks
  out
}


#' @title Default checks for labelled variables
#'
#' @param remove Character vector of function names. Checks to remove from the returned vector 
#' 
#' @param add Character vector of function names. Checks to add to the returned vector
#'
#' @description Default options for which checks to perform on
#' labelled type variables in \code{\link{check}} and \code{\link{makeDataReport}},
#' possibly user-modified by adding extra function names using \code{add} or 
#' removing default function names with \code{remove}. 
#'
#' @return A vector of function names.
#'
#' @export
defaultLabelledChecks <- function(remove = NULL, add = NULL) {
  defVals <- c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                      "identifyCaseIssues", "identifyNums")
  unique(c(setdiff(defVals, remove), add))
}


#' @export
check.labelled <- function(v, nMax =  10, checks = setChecks(),
                           labelledChecks = NULL, ...) {
  if (is.null(labelledChecks)) labelledChecks <- checks$labelled
  out <- lapply(labelledChecks, function(x) eval(call(x, v = v, nMax = nMax)))
  names(out) <- labelledChecks
  out
}


#' @title Default checks for numeric variables
#'
#' @param remove Character vector of function names. Checks to remove from the returned vector 
#' 
#' @param add Character vector of function names. Checks to add to the returned vector
#'
#' @description Default options for which checks to perform on
#' numeric type variables in \code{\link{check}} and \code{\link{makeDataReport}},
#' possibly user-modified by adding extra function names using \code{add} or 
#' removing default function names with \code{remove}. 
#'
#' @return A vector of function names.
#'
#' @export
defaultNumericChecks <- function(remove = NULL, add = NULL) {
  defVals <- c("identifyMissing", "identifyOutliers")
  unique(c(setdiff(defVals, remove), add))
}

#' @export
check.numeric <- function(v, nMax =  10, checks = setChecks(),
                          maxDecimals = 2,
                          numericChecks = NULL, ...) {
  if (is.null(numericChecks)) numericChecks <- checks$numeric
  out <- lapply(numericChecks, function(x) eval(call(x, v = v, nMax = nMax,
                                              maxDecimals = maxDecimals)))
  names(out) <- numericChecks
  out
}


#' @title Default checks for integer variables
#'
#' @param remove Character vector of function names. Checks to remove from the returned vector 
#' 
#' @param add Character vector of function names. Checks to add to the returned vector
#'
#' @description Default options for which checks to perform on
#' integer type variables in \code{\link{check}} and \code{\link{makeDataReport}},
#' possibly user-modified by adding extra function names using \code{add} or 
#' removing default function names with \code{remove}. 
#'
#' @return A vector of function names.
#'
#' @export
defaultIntegerChecks <- function(remove = NULL, add = NULL) {
  defVals <- c("identifyMissing", "identifyOutliers")
  unique(c(setdiff(defVals, remove), add))
}

#' @export
check.integer <- function(v, nMax =  10, checks = setChecks(),
                          maxDecimals = 2,
                          integerChecks = NULL, ...) {
  if (is.null(integerChecks)) integerChecks <- checks$integer
  out <- lapply(integerChecks, function(x) eval(call(x, v = v, nMax = nMax,
                                              maxDecimals = maxDecimals)))
  names(out) <- integerChecks
  out
}



#' @title Default checks for logical variables
#'
#' @param remove Character vector of function names. Checks to remove from the returned vector 
#' 
#' @param add Character vector of function names. Checks to add to the returned vector
#'
#' @description Default options for which checks to perform on
#' logical type variables in \code{\link{check}} and \code{\link{makeDataReport}},
#' possibly user-modified by adding extra function names using \code{add} or 
#' removing default function names with \code{remove}. 
#'
#' @return A vector of function names.
#'
#' @export
defaultLogicalChecks <- function(remove = NULL, add = NULL) {
  defVals <- NULL 
  unique(c(setdiff(defVals, remove), add))
}

#' @export
check.logical <- function(v, nMax =  10, checks = setChecks(),
                          logicalChecks = NULL, ...) {
  if (is.null(logicalChecks)) logicalChecks <- checks$logical
  if (!is.null(logicalChecks)) {
    out <- lapply(logicalChecks, function(x) eval(call(x, v=v, nMax = nMax)))
    names(out) <- logicalChecks
    return(out)
  } else return(list(NoChecksPerformed = checkResult(list(problem = FALSE,
                                                        message = "",
                                                        problemValues = NULL))))
}


#' @title Default checks for Date variables
#'
#' @param remove Character vector of function names. Checks to remove from the returned vector 
#' 
#' @param add Character vector of function names. Checks to add to the returned vector
#'
#' @description Default options for which checks to perform on
#' Date type variables in \code{\link{check}} and \code{\link{makeDataReport}},
#' possibly user-modified by adding extra function names using \code{add} or 
#' removing default function names with \code{remove}. 
#'
#' @return A vector of function names.
#'
#' @export
defaultDateChecks <- function(remove = NULL, add = NULL) {
  defVals <- "identifyOutliers"
  unique(c(setdiff(defVals, remove), add))
}

#' @export
check.Date <- function(v, nMax =  10, checks = setChecks(),
                       DateChecks = NULL, ...) {
  if (is.null(DateChecks)) DateChecks <- checks$Date
  if (! is.null(DateChecks)) {
    out <- lapply(DateChecks, function(x) eval(call(x, v=v, nMax = nMax)))
    names(out) <- DateChecks
    return(out)
  } else return(list(NoChecksPerformed = checkResult(list(problem = FALSE,
                                                        message = "",
                                                        problemValues = NULL))))
}



#' @export
check.data.frame <- function(v, nMax = 10, checks = setChecks(),
                             maxDecimals = 2, ...) {
  lapply(v, check, nMax = nMax, checks = checks, maxDecimals = maxDecimals, ...)
}



##########################################Not exported below#########################################

#empty

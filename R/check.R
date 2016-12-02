#Functions called from clean.R for performing cleaning steps/checks
#TO DO: structure: functions that are to check whether a problem is present
#             for each variable return a checkRes object, consisting of a
#             boolean and a message text (possibly empty) (=> define class checkRes)
#TO DO: Consider: Should the "v <- na.omit(v)" command at the beginning of
#       some (all?) checking functions be moved to the check-functions
#       themselves? I.e. do we ever need to consider missing values when checking?
#   And similarly: Should the labelled-vector unpacking also just be done once?
#TO DO: What should check() output? Maybe a list of checkRes-objects, and then
#       I should also implement a print() method for checkRes-lists to be used in clean()

#General comments:
# arguments: v is a variable (column) from a data.frame, i.e. a vector
# note: labelled is Wickham's class from the haven/hmisc packages
# note: Function names are suffixed with an indicator of the data types it
#       is to be called on. N is numerical, I is integer, F is factor,
#       L is labeled (Wickham), C is character. The ordering of suffix
#       letters is arbitrary. All suffixed functions are purely internal
#       and their calling is controlled using s3 methods


##########################################

####Check#####

#' Perform a check of potential errors in a data frame
#'
#' Runs a set of validation checks to check a vector for potential errors.  performs checking steps according to user input and/or data type of the inputted variable.
#'
#' @param v the vector to be checked
#' @inheritParams clean
#' @param \dots other arguments that are passed on the to checking functions
#' @return An list object of class "checked" summarizing the result from the check of the check. As a minimum the returned class should contain the following elements
#' \itemize{
#'   \item{"name"}{The name of the check}
#'   \item{"description"}{Slightly more information about the check}
#'   \item{"problem"}{An integer giving an error code. 0 means no potential errors were identified}
#'   \item{"message"}{A string giving summary information in R markdown format about the results}
#' }
#' @author Anne H. Petersen \email{ahpe@@sund.ku.dk} and Claus Thorn Ekstrom \email{ekstrom@@sund.ku.dk}
#' @seealso \code{\link{clean}}
#' @keywords misc
#' @examples
#'
#' x <- 1:5
#' check(x)
#'
#' # Annoyingly coded missing as 99
#' y <- c(rnorm(100), rep(99, 10))
#'
#' @importFrom utils packageVersion tail
#' @export
check <- function(v, nMax = Inf, ...) UseMethod("check")

#characterChecks <- function() {
#    list(identifyMissing, identifyWhitespace, identifyLoners, identifyCaseIssues)
#}

#factorChecks <- function() {
#list(identifyMissing,
#     identifyWhitespace,
#     identifyLoners,
#     identifyCaseIssues)
#}

#Overwriting Claus' version so that it is consistent with what I did in
#visualize and summarize. Either change everything to Claus' method
#or keep my version everywhere.

#check.character <- function(v, characterChecks=NULL, ...) {
#    lapply(characterChecks, function( runthis ) { runthis(v, ...) })
#}

#check.factor <- function(v, factorChecks=NULL, ...) {
#    lapply(factorChecks, function( runthis ) { runthis(v, ...) })
#}


#' @export
defaultCharacterChecks <- function() c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                       "identifyCaseIssues", "identifyNums")
#' @export
check.character <- function(v, nMax =  Inf, characterChecks=defaultCharacterChecks(), ...) {
  out <- lapply(characterChecks, function(x) eval(call(x, v=v, nMax = nMax)))
  names(out) <- characterChecks
  out
}


#' @export
defaultFactorChecks <- function() c("identifyMissing", "identifyWhitespace", "identifyLoners",
                                       "identifyCaseIssues", "identifyNums")
#' @export
check.factor <- function(v, nMax =  Inf, factorChecks = defaultFactorChecks(), ...) {
  out <- lapply(factorChecks, function(x) eval(call(x, v=v, nMax = nMax)))
  names(out) <- factorChecks
  out
}


#' @export
defaultLabelledChecks <- function() c("identifyMissing", "identifyWhitespace")

#' @export
check.labelled <- function(v, nMax =  Inf, labelledChecks = defaultLabelledChecks(), ...) {
  out <- lapply(labelledChecks, function(x) eval(call(x, v=v, nMax = nMax)))
  names(out) <- labelledChecks
  out
}


#' @export
defaultNumericChecks <- function() c("identifyMissing", "identifyOutliers")

#' @export
check.numeric <- function(v, nMax =  Inf, maxDecimals = 2,
                          numericChecks = defaultNumericChecks(), ...) {
  out <- lapply(numericChecks, function(x) eval(call(x, v=v, nMax = nMax,
                                              maxDecimals = maxDecimals)))
  names(out) <- numericChecks
  out
}


#' @export
defaultIntegerChecks <- function() c("identifyMissing", "identifyOutliers")

#' @export
check.integer <- function(v, nMax =  Inf, maxDecimals = 2,
                          integerChecks = defaultIntegerChecks(), ...) {
  out <- lapply(integerChecks, function(x) eval(call(x, v=v, nMax = nMax,
                                              maxDecimals = maxDecimals)))
  names(out) <- integerChecks
  out
}




#' @export
defaultLogicalChecks <- function() NULL #NOTE: we don't actually do any logical checks...

#' @export
check.logical <- function(v, nMax =  Inf, logicalChecks = defaultLogicalChecks(), ...) {
  if (! is.null(logicalChecks)) {
    out <- lapply(logicalChecks, function(x) eval(call(x, v=v, nMax = nMax)))
    names(out) <- logicalChecks
    return(out)
  } else return(list(NoChecksPerformed=list(problem = FALSE, message="")))
}


#' @export
defaultDateChecks <- function() NULL #NOTE: we don't actually do any logical checks...

#' @export
check.Date <- function(v, nMax =  Inf, DateChecks = defaultDateChecks(), ...) {
  if (! is.null(DateChecks)) {
    out <- lapply(DateChecks, function(x) eval(call(x, v=v, nMax = nMax)))
    names(out) <- DateChecks
    return(out)
  } else return(list(NoChecksPerformed=list(problem = FALSE, message="")))
}





##########################################Not exported below#########################################


#' @title summaryFunction for unique values
#'
#' @description A \code{\link{summaryFunction}} type function, intended to be called from 
#' \code{\link{summarize}} to be called from \code{\link{summarize}}, which counts the
#' number of unique (excluding \code{NA}s) values in a variable.
#'
#' @param v A variable (vector).
#'
#' @param ... Not in use.
#'
#' @return An object of class \code{summaryResult} with the following entries: 
#' \code{$feature} ("No. unique values") and \code{$result} (the number of unique 
#' values in \code{v}).
#'
#' @seealso \code{\link{summaryFunction}}, \code{\link{summarize}}, \code{\link{summaryResult}},
#' \code{\link{allSummaryFunctions}}
#'
#' @examples
#' uniqueValues(c(1:3, rep(NA, 10), Inf, NaN))
#'
#' @importFrom stats na.omit
#' @export
uniqueValues <- function(v, ...) UseMethod("uniqueValues")

#assign methods to generic uniqueValues function

#' @export
uniqueValues.character <- function(v, ...) uniqueValuesCFLBI(v)

#' @export
uniqueValues.factor <- function(v, ...) uniqueValuesCFLBI(v)

#' @export
uniqueValues.labelled <- function(v, ...) uniqueValuesCFLBI(v) #?PROBLEM?

#' @export
uniqueValues.numeric <- function(v, ...) uniqueValuesN(v)

#' @export
uniqueValues.integer <- function(v, ...) uniqueValuesCFLBI(v)

#' @export
uniqueValues.logical <- function(v, ...) uniqueValuesCFLBI(v)

#' @export
uniqueValues.Date <- function(v, ...) uniqueValuesCFLBI(v)


#Make it a summaryFunction
#' @include summaryFunction.R
#' @export
uniqueValues <- summaryFunction(uniqueValues, "Count number of unique values", allClasses())


##########################################Not exported below#########################################


#methods for each variable type
uniqueValuesCFLBI <- function(v) {
  noUnique <- length(unique(na.omit(v)))
  summaryResult(list(feature="Number of unique values",
                     result = noUnique,
                     value = noUnique))
}

uniqueValuesN <- function(v) {
  out <- uniqueValuesCFLBI(v)

  #check for NaNs
  if (any(is.nan(v))) out$result <- out$result + 1

  out
}



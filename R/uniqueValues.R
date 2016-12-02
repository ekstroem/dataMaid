#' @title Summary function for unique values
#'
#' @description A summary type function to be called from \code{\link{summarize}}, which counts the
#' number of unique (excluding NA) values in a variable.
#'
#' @param v A variable (vector).
#'
#' @param ... Not in use.
#'
#' @return A list with $feature: "No. unique values" and
#' $result: [the number of unique values].
#'
#' @seealso \code{\link{summarize}}
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
#' @export
uniqueValues <- summaryFunction(uniqueValues, "Count number of unique values")


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



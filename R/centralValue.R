#' @title Description function for central values
#'
#' @description A description type function to be called from \code{\link{summarize}}, which returns
#' the central value of a variable. For numeric and integer variables, this is the median. For
#' character, factor, labelled and logical variables, the central value is the mode (i.e. the
#' value that occurs the largest number of times).
#'
#' @param v A variable (vector).
#'
#' @param ... Extra arguments to be passed to class-specific functions. These incluse
#' \code{maxDecimals} (default is 2) which controls the rounding of integer and numeric
#' values.
#'
#' @details Note that NA, NaN and Inf values are ignored for numeric and integer variables, while
#' only NA values are ignored for factor, character and labelled variables. No values are
#' ignored for logical variables.
#'
#' @return A list with $feature: [Mode/median] and $result: [the central value of \code{v}].
#' WRITE SOMETHING ABOUT WHAT HAPPENS WHEN MODE IS NOT UNIQUE.
#'
#' @seealso \code{\link{summarize}}
#'
#' @examples
#'  #central value of a integer variable:
#'    centralValue(c(rep(1, 25), rep(2, 10), rep(3, 20)))
#'
#'  #central value of a character variable:
#'    centralValue(as.character(c(rep(1, 20), rep(2, 10), rep(3, 20))))
#'
#' @importFrom stats na.omit median
#' @export
centralValue <- function(v, ...) UseMethod("centralValue")
centralValue <- summaryFunction(centralValue, "Compute median or mode")

#assign methods to generic centralValue function

#' @export
centralValue.character <- function(v, ...) centralValueCF(v)

#' @export
centralValue.factor <- function(v, ...) centralValueCF(v)

#' @export
centralValue.labelled <- function(v, ...) centralValueL(v)

#' @export
centralValue.numeric <- function(v, ...) centralValueIN(v, ...)

#' @export
centralValue.integer <- function(v, ...) centralValueIN(v, ...)

#' @export
centralValue.logical <- function(v, ...) centralValueB(v)

#' @export
centralValue.Date <- function(v, ...) centralValueCF(v)



##########################################Not exported below#########################################


#methods for each variable type

#logical variables
centralValueB <- function(v) {
  vMode <- names(which.max(table(v, exclude=NULL)))[1]
  summaryResult(list(feature="Mode",
                     result=paste("\"", vMode, "\"", sep=""),
                     value = vMode))
}

#character and factor variables
##' @importFrom stats na.omit
centralValueCF <- function(v) {
  centralValueB(na.omit(v))
}

#labelled variables
centralValueL <- function(v) {
  #PLACE HOLDER
  list(feature="Mode", result="?labelled?")
}

#integer and numeric variables
##' @importFrom stats median na.omit
centralValueIN <- function(v, maxDecimals = 2) {
  v <- na.omit(v)
  val <- median(v)
  summaryResult(list(feature="Median",
                     result=round(val, maxDecimals), value = val))
}




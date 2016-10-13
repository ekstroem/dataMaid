#' @title Description function for central values
#'
#' @description A description type function to be called from \code{\link{summarize}}, which returns
#' the central value of a variable. For numeric and integer variables, this is the median. For
#' character, factor, labelled and logical variables, the central value is the mode (i.e. the
#' value that occurs the largest number of times).
#'
#' @param v A variable (vector).
#'
#' @details Note that NA, NaN and Inf values are ignored for numeric and integer variables, while
#' only NA values are ignored for factor, character and labelled variables. No values are
#' ignored for logical variables.
#'
#' @return A list with $feature: [Mode/median] and $result: [the central value of \code{v}].
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
centralValue <- function(v) UseMethod("centralValue")



##########################################Not exported below#########################################


#methods for each variable type

#logical variables
centralValueB <- function(v) {
  vCats <- unique(v)
  vMode <- vCats[which.max(table(v, exclude=NULL))][1]
  list(feature="Mode", result=paste("\"", vMode, "\"", sep=""))
}

#character and factor variables
# #' @importFrom stats median
centralValueCF <- function(v) {
  centralValueB(na.omit(v))
}

#labelled variables
centralValueL <- function(v) {
  #PLACE HOLDER
  list(feature="Mode", result="?labelled?")
}

#integer and numeric variables
# #' @importFrom stats median
centralValueIN <- function(v) {
  v <- na.omit(v)
  list(feature="Median", result=median(v))
}


#assign methods to generic centralValue function
#' @export
centralValue.character <- function(v) centralValueCF(v)
#' @export
centralValue.factor <- function(v) centralValueCF(v)
#' @export
centralValue.labelled <- function(v) centralValueL(v)
#' @export
centralValue.numeric <- function(v) centralValueIN(v)
#' @export
centralValue.integer <- function(v) centralValueIN(v)
#' @export
centralValue.logical <- function(v) centralValueB(v)




#' @title summaryFunction that finds reference level for factor variables
#'
#' @description A \code{summaryFunction}, intended to be called from 
#' \code{\link{summarize}}, which returns the reference level of a factor variable, 
#' i.e. the first category as returned by \code{levels(v)}. This level will serve
#' as the reference category and get absorbed into the intercept for most standard 
#' model fitting procedures and therefore, it may be convenient to know. 
#' 
#' @param v A variable (vector) of type factor.
#'
#' @param ... Not in use.
#' 
#' @return An object of class \code{summaryResult} with the following entries: \code{$feature} 
#' ("Reference level"), \code{$result} (the reference level of \code{v}), and \code{$value}
#' (identical to result).
#'
#' @seealso \code{\link{summaryFunction}}, \code{\link{summarize}}, \code{\link{summaryResult}},
#' \code{\link{allSummaryFunctions}}
#' 
#' @examples
#' refCat(factor(letters))
#'
#' @importFrom stats na.omit
#' @export
refCat <- function(v, ...) {
  val <- levels(v)[1]
  res <- val
  summaryResult(list(feature = "Reference category", result = res,
                     value = val))
}

#Make it a summaryFunction
#' @include summaryFunction.R
refCat <- summaryFunction(refCat, "Find reference level",
                          c("factor"))

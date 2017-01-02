#' @title Summary function for original class
#'
#' @description A \code{\link{summaryFunction}} type function, intended to be called from
#' \code{\link{summarize}} to be called from \code{\link{summarize}}, which finds the
#' orignial class of a variable. This is just the class for all objects but those of class
#' \code{smartNum}.
#'
#' @param v A variable (vector).
#'
#' @param ... Not in use.
#'
#' @return An object of class \code{summaryResult} with the following entries:
#' \code{$feature} ("Variable type"), \code{$result} (the (original) class of
#' \code{v}) and \code{$value} (identical to \code{$result}).
#'
#' @seealso \code{\link{summarize}}
#'
#' @examples
#'  #For standard variables:
#'    varX <- c(rep(c(1,2,3), each=10))
#'    class(varX)
#'    variableType(varX)
#'
#'  #For smartNum variables:
#'    smartX <- dataMaid:::smartNum(varX)
#'    class(smartX)
#'    variableType(smartX)
#'
#' @include smartNum.R
#' @export
variableType <- function(v, ...) {
  vClass <- oClass(v)[1]
  summaryResult(list(feature="Variable type", result = vClass, value = vClass))
}

#' @include variableType.R
variableType <- summaryFunction(variableType, "Data class of variable", allClasses())

#' @title Description function for quartiles
#'
#' @description A description type function to be called from \code{\link{summarize}}, which calculates
#' the 1st and 3rd quartiles of a variable. NA, NaN and Inf values are removed prior to  the
#' computations.
#'
#' @param v A variable (vector) of type numeric or integer.
#' 
#' @inheritParams clean
#'
#' @return A list with $feature: "1st and 3rd quartiles" and $result: [the quartiles of \code{v}].
#'
#' @seealso \code{\link{summarize}}
#'
#' @examples
#' quartiles(c(1:100))
#'
#' @importFrom stats na.omit quantile
#' @export
quartiles <- function(v, maxDecimals = 2) {
  v <- na.omit(v) #maybe keep Inf's?
  list(feature="1st and 3rd quartiles", result = paste(round(quantile(v, c(0.25, 0.75)), 
                                                             maxDecimals), 
                                                       collapse="; "))
}
quartiles <- summaryFunction(quartiles, "Compute 1st and 3rd quartiles",
                             classes = c("integer", "numeric"))



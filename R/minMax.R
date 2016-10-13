#' @title Description function for minimum and maximum
#' 
#' @description A description type function to be called from \code{\link{summarize}}, which returns
#' the minimum and maximum value of a variable. NA, NaN and Inf values are removed prior to  the 
#' computations.
#' 
#' @param v A variable (vector) of type numeric or integer.
#' 
#' @return A list with $feature: "Min. and max." and $result: [the minimum and maximum of \code{v}]. 
#' 
#' @seealso \code{\link{summarize}}
#' 
#' @examples
#' minMax(c(1:100))
#' 
#' @export
minMax <- function(v) {
  v <- na.omit(v) #maybe keep Inf's?
  list(feature="Min. and max.", result=paste(min(v), max(v), sep=", "))
}

#' @title summaryFunction for quartiles
#'
#' @description A \code{\link{summaryFunction}}, intended to be called from \code{\link{summarize}}, 
#' which calculates the 1st and 3rd quartiles of a variable. NA, NaN and Inf values are removed 
#' prior to  the computations.
#' 
#' @param v A variable (vector) of type numeric or integer.
#' 
#' @inheritParams makeDataReport
#' 
#' @details The quartiles are computed using the \code{\link[stats]{quantile}} function from \code{stats}, 
#' using type 7 quantiles for integer and numeric variables and type 1 quantiles for Date variables. 
#'
#' @return An object of class \code{summaryResult} with the following entries: \code{$feature} 
#' ("1st and 3rd quartiles"), \code{$result} (the 1st and 3rd quartiles of \code{v}) and
#' \code{$value} (the quartiles in their original format).
#'
#' @seealso \code{\link{summaryFunction}}, \code{\link{summarize}}, \code{\link{summaryResult}},
#' \code{\link{allSummaryFunctions}}
#'
#' @examples
#' quartiles(c(1:100))
#' 
#' quartiles(rnorm(1000), maxDecimals = 4)
#'
#' @importFrom stats na.omit quantile
#' @export
quartiles <- function(v, maxDecimals = 2) {
  v <- na.omit(v) #maybe keep Inf's?
  quants <- quantile(v, c(0.25, 0.75),
                     type = ifelse("Date" %in% class(v),
                                   1, 7))
  summaryResult(list(feature="1st and 3rd quartiles", 
                     result = paste(round(quants, maxDecimals), 
                                    collapse="; "),
                     value = c(quants)))
}

#' @include summaryFunction.R
quartiles <- summaryFunction(quartiles, "Compute 1st and 3rd quartiles",
                             classes = c("Date", "integer", "numeric"))



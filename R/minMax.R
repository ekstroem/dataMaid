#' @title summaryFunction for minimum and maximum
#'
#' @description A \code{summaryFunction}, intended to be called from 
#' \code{\link{summarize}}, which returns the minimum and maximum values of a variable.
#' NA, NaN and Inf values are removed prior to the computations.
#' 
#' @param v A variable (vector) of type numeric or integer.
#'
#' @inheritParams makeDataReport
#' 
#' @return An object of class \code{summaryResult} with the following entries: \code{$feature} 
#' ("Min. and max."), \code{$result} (the minimum and maximum of \code{v}), and \code{$value}
#' (minimum and maximum in their orignial format).
#'
#' @seealso \code{\link{summaryFunction}}, \code{\link{summarize}}, \code{\link{summaryResult}},
#' \code{\link{allSummaryFunctions}}
#' 
#' @examples
#' minMax(c(1:100))
#'
#' @importFrom stats na.omit
#' @export
minMax <- function(v, maxDecimals = 2) {
    v <- na.omit(v) #maybe keep Infs instead?
    if(length(v)>0) {
        minV <- min(v)
        maxV <- max(v)
    } else {
        minV <- NA
        maxV <- NA            
    }
    summaryResult(list(feature="Min. and max.",
                       result=paste(round(minV, maxDecimals),
                                    round(maxV, maxDecimals),
                                    sep="; "),
                       value = c(minV, maxV)))
}

#Make it a summaryFunction
#' @include summaryFunction.R
minMax <- summaryFunction(minMax, "Find minimum and maximum values",
                          c("integer", "numeric", "Date"))

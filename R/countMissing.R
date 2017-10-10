#' Summary function for missing values
#'
#' A \code{\link{summaryFunction}}, intended to be called from
#' \code{\link{summarize}} (and \code{\link{makeDataReport}}), which counts the
#' number of missing (\code{NA}) values in a variable.
#'
#' @param v A variable (vector).
#' @param ... Not in use.
#'
#' @return A \code{\link{summaryResult}} object with the following entries: 
#' \code{$feature} ("No. missing obs."), \code{$result} (the number and percentage 
#' missing observations) and \code{$value} (the number of missing observations).
#'
#' @seealso \code{\link{summarize}}, \code{\link{allSummaryFunctions}},
#' \code{\link{summaryFunction}}, \code{\link{summaryResult}}
#'
#' @examples
#' countMissing(c(1:100, rep(NA, 10)))
#'
#' @export
countMissing <- function(v, ...) {
  noMissing <- sum(is.na(v))
  percentMissing <- round(100*noMissing/length(v),2)
  summaryResult(list(feature = "Number of missing obs." ,
                     result = paste(noMissing, " (",
                                    percentMissing," %)", sep=""),
                     value = noMissing))
}

#' @include summaryFunction.R
countMissing <- summaryFunction(countMissing,
                                "Compute proportion of missing observations",
                                allClasses())

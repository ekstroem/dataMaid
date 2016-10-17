#' @title Summary function for missing values
#'
#' @description A summary type function to be called from \code{\link{summarize}}, which counts the
#' number of missing (NA) values in a variable.
#'
#' @param v A variable (vector).
#'
#' @return A list with $feature: "No. missing obs." and
#' $result: [the number and percentage missing observations].
#'
#' @seealso \code{\link{summarize}}
#'
#' @examples
#' countMissing(c(1:100, rep(NA, 10)))
#'
#' @export
countMissing <- function(v) {
  noMissing <- sum(is.na(v))
  percentMissing <- round(100*noMissing/length(v),2)
  list(feature="No. missing obs." ,
       result=paste(noMissing, " (", percentMissing," %)", sep=""))
}
countMissing <- summaryFunction(countMissing, "Compute ratio of missing observations",
                                c("character", "factor", "integer", "labelled", "logical", 
                                  "numeric"))

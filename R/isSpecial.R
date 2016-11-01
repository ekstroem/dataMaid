#Check if a variable is a key (all observations have unique values) or
#"empty" (only one unique value)
#Maybe deal with missing values? They count as distinct values right now...

#document me! Also, maybe: split into two functions, one for keys and one for empty
#' @export
isSpecial <- function(v) {
  out <- list(problem=F, status="", message="")
  nVals <- length(unique(v))
  if (nVals == 1) {
    out <- list(problem=T, status="empty",
                message=paste("The variable only takes one value: \"", v[1],
                              "\".", sep=""))
  } else if (nVals == length(v) & !any(class(v) %in% c("numeric", "integer"))) {
    out <- list(problem=T, status="key",
                message="The variable is a key (distinct values for each observation).")
  }
  out
}
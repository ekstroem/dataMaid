#' @export
identifyNums <- function(v) {
  out <- list(problem = FALSE, message = "")
  v <- as.character(na.omit(v))
  if (length(unique(v)) <= 11) {
    return(out)
  }
  v[v==""] <- "a" 
  v <- gsub("[[:digit:]]", "", v)
  if (sum(nchar(v)) == 0) {
    out$problem <- TRUE
    out$message <- "Note: The variable consists only of numbers and has a lot of different unique values. Is it perhaps a misclassified numeric variable?"
  }
  out
}

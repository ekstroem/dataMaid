#' @export
identifyNums <- function(v, ...) {
  out <- list(problem = FALSE, message = "", problemValues = NULL)
  v <- as.character(na.omit(v))
  if (length(unique(v)) <= 11) {
    return(checkResult(out))
  }
  v[v==""] <- "a" #make sure v contains no empty strings
  v <- gsub("^-{1}", "", v) #remove signs (prefixed -)
  v <- gsub("[[:digit:]]", "", v) #replace numbers with empty strings
  if (sum(nchar(v)) == 0) {
    out$problem <- TRUE
    out$message <- "Note: The variable consists exclusively of numbers and takes a lot of different values. Is it perhaps a misclassified numeric variable?"
  }
  checkResult(out)
}

identifyNums <- checkFunction(identifyNums, "Identify misclassified numeric or integer variables",
                              c("character", "factor"))

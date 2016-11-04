#' @export
identifyNums <- function(v, ...) {
  out <- list(problem = FALSE, message = "")
  v <- as.character(na.omit(v))
  if (length(unique(v)) <= 11) {
    return(out)
  }
  v[v==""] <- "a" #make sure v contains no empty strings
  v <- gsub("^-{1}", "", v) #remove signs (prefixed -)
  v <- gsub("[[:digit:]]", "", v) #replace numbers with empty strings
  if (sum(nchar(v)) == 0) {
    out$problem <- TRUE
    out$message <- "Note: The variable consists exclusively of numbers and takes a lot of different values. Is it perhaps a misclassified numeric variable?"
  }
  out
}

identifyNums <- checkFunction(identifyNums, "Identify missclassified numeric or integer variables",
                              c("character", "factor"))

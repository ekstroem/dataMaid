

#' @export
checkResult <- function(ls) {
  if (length(setdiff(names(ls), c("problem", "message", "problemValues"))) != 0) {
    stop("Hm, what's informative here? Describe issue...")
  } else {
    #maybe check here if ls is consistent? If problem = F, message
    #should be empty and problemValues too
    class(ls) <- "checkResult"
  }
  ls
}


#' @export
print.checkResult <- function(x, ...) {
  if (x$problem) {
    mes <- x$message
  } else mes <- "No problems found."
  
  #remove escaping and quoting designed for rmarkdown rendering
  mes <- gsub("\\\\\"", "", mes)
  
  cat(mes)
}
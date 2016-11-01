#' @export
allXFunctions <- function(X) {
  #.GlobalEnv isn't the right place to look. Got to add stuff loaded
  #from packages too!
  #e <- as.environment("package:cleanR")
  #parent.env(e) <- .GlobalEnv
  allF <- Filter(function(x) X %in% class(get(x)), union(ls(envir = .GlobalEnv), 
                                                         ls("package:cleanR")))
  out <- list(name = allF, description = sapply(allF, function(x) description(get(x))),
              classes = lapply(allF, function(x) classes(get(x))))
  class(out) <- c("functionSummary", "list")
  out
}
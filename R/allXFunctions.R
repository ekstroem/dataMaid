#Make a functionSummary of all functions of type X, that is,
#coerce their names, description attributes and classes attributes
#into a list.
#Called from allVisualFunction(), allSummaryFunctions(), allCheckFunctions().
allXFunctions <- function(X) {
  allF <- Filter(function(x) X %in% class(get(x)), union(ls(envir = .GlobalEnv),
                                                         ls("package:dataMaid")))
  out <- list(name = allF, description = sapply(allF, function(x) description(get(x))),
              classes = lapply(allF, function(x) classes(get(x))))
  class(out) <- c("functionSummary", "list")
  out
}


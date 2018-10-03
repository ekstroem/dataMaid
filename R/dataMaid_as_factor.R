#Code from haven but preserved local copy to ensure backwards compatability
#for old version of labelled class
#Copied from github on sep 17 2018
dataMaid_as_factor <- function(x, levels = c("default", "labels", "values", "both"),
                               ordered = FALSE, ...) {
  levels <- match.arg(levels)
  label <- attr(x, "label", exact = TRUE)
  labels <- attr(x, "labels")
  
  if (levels == "default" || levels == "both") {
    if (levels == "both") {
      names(labels) <- paste0("[", labels, "] ", names(labels))
    }
    
    # Replace each value with its label
    vals <- unique(x)
    levs <- haven:::replace_with(vals, unname(labels), names(labels))
    # Ensure all labels are preserved
    levs <- sort(c(stats::setNames(vals, levs), labels), na.last = TRUE)
    levs <- unique(names(levs))
    
    x <- haven:::replace_with(x, unname(labels), names(labels))
    
    x <- factor(x, levels = levs, ordered = ordered)
  } else {
    levs <- unname(labels)
    labs <- switch(levels,
                   labels = names(labels),
                   values = levs
    )
    x <- haven:::replace_with(x, levs, labs)
    x <- factor(x, labs, ordered = ordered)
  }
  
  structure(x, label = label)
}
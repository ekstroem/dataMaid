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
    levs <- dataMaid_haven_replace_with(vals, unname(labels), names(labels))
    # Ensure all labels are preserved
    levs <- sort(c(stats::setNames(vals, levs), labels), na.last = TRUE)
    levs <- unique(names(levs))
    
    x <- dataMaid_haven_replace_with(x, unname(labels), names(labels))
    
    x <- factor(x, levels = levs, ordered = ordered)
  } else {
    levs <- unname(labels)
    labs <- switch(levels,
                   labels = names(labels),
                   values = levs
    )
    x <- dataMaid_haven_replace_with(x, levs, labs)
    x <- factor(x, labs, ordered = ordered)
  }
  
  structure(x, label = label)
}


## Adding a verbatim copy of the unexported function dataMaid_haven_replace_with
## so a note does not pop up when checking the package.
dataMaid_haven_replace_with <- function(x, from, to) 
{
    stopifnot(length(from) == length(to))
    out <- x
    matches <- match(x, from, incomparables = NA)
    out[!is.na(matches)] <- to[matches[!is.na(matches)]]
    tagged <- haven::is_tagged_na(x)
    if (!any(tagged)) {
        return(out)
    }
    matches <- match(haven::na_tag(x), haven::na_tag(from), incomparables = NA)
    out[!is.na(matches)] <- to[matches[!is.na(matches)]]
    out
}

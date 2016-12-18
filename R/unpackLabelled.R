#Maybe delete? Deal when labelled stuff is implemented.
#this function is used in identifyMissing.R and
#identifyWhitespace.R
unpackLabelled <- function(v) {
  c(as.character(v), attributes(v)$labels)
}



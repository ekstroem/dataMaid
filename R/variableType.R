#####variableType######

#Returns the class of the variable, as registered in R.

variableType <- function(v) {
  list(feature="Variable type", result=oClass(v))
}
#oClass is defined in clean.R - is this a problem?

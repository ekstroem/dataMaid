#Produce a message using the internal output from a checking-function,
#i.e. a named list(problem=...(boolean), problemValues=...(string vector))
#NOTE: 3 slashes escapes the espaced string [\"] such that it is printed correctly
#(and not intepreted) in markdown.

#document me!
#make more suitable for users to call this function
#' @export
messageGenerator <- function(problemStatus, check = NULL, messages = NULL, 
                             message = NULL) {
  if (is.null(messages) & !is.null(check)) {
    messages <- list(identifyMiss =
                       "The following suspected missing value codes enter as regular values:",
                     identifyWhitespace =
                       "The following values appear with prefixed or suffixed white space:",
                     identifyOutliers =
                       "Note that the following possible outlier values were detected:",
                     identifyLoners =
                       "Note that the following levels have at most five observations:",
                     identifyCaseIssues =
                       "Note that there might be case problems with the following levels:")
  } else {
    #ideally: make a standard message if no argument is supplied by identifying the name 
    #of the function that called messageGenerator and thereby writing something like
    #"[functionName] problem values: [problemValues]"
    messages <- list(onlyMessage = message)
    check <- "onlyMessage"
  }
  
  ifelse(problemStatus$problem,
         paste(messages[[check]], paste(paste("\\\"", sort(problemStatus$problemValues), "\\\"", sep=""),
                                        collapse=", ")),
         "")
}


#Make a function of type X (among visualFunction, summaryFunction and
#checkFunction). If classes is null, and fName is a S3 generic,
#the function will make a look-up for all methods available in the
#global enviroment and fill out the slot accordingly.
#Note: This does not work for functions defined wihtin cleanR. As
#of now, they must have their classes specified explicitly!
#' @importFrom utils methods
makeXFunction <- function(fName, description, classes, X) {
  f <- get(fName)

  if (is.null(description)) description <- fName
  if (is.null(classes)) {
    theseMethods <- as.character(methods(fName)) #methods() needs the name in order
    #to work inside the function

    #if (length(theseMethods) == 0) {
    #  callEnv <- parent.env(as.environment(-1L))
    #  theseMethods <- as.character(utils::.S3methods(fName, envir = callEnv))
    #}

    if (length(theseMethods) > 0) {
      classes <- sub(paste(fName, ".", sep=""),
                     "", theseMethods)
    } else classes <- character()
  }
  class(f) <- c(X, "function")
  attr(f, "description") <- description
  attr(f, "classes") <- classes
  f
}




#foo <- function(x) UseMethod("foo")
#
#foo.character <- function(x) x
#foo.numeric <- function(x) x + 1
#
#makeAttributedFunction <- function(fName, classes = NULL) {
#  if (is.null(classes)) {
#    browser()
#    theseMethods <- as.character(methods(fName))
#    if (length(theseMethods) > 0) {
#      classes <- sub(paste(fName, ".", sep=""),
#                     "", theseMethods)
#    } else classes <- character()
#  }
#  f <- get(fName)
#  attr(f, "classes") <- classes
#  class(f) <- "attributedFunction"
#  f
#}
#
#foo <- makeAttributedFunction("foo")



# '   @e xport
#'@importFrom utils .S3methods methods
makeXFunction <- function(fName, description, classes, X) {
  f <- get(fName)
  if (is.null(classes)) {
    #browser()
    theseMethods <- as.character(utils::methods(fName)) #methods() needs the name in order
    if (length(theseMethods) == 0) {
      callEnv <- parent.env(as.environment(-1L))
      theseMethods <- as.character(utils::.S3methods(fName, envir = callEnv))
    }
    #to work inside the function
    #print(standardVisual)
    #browser()
    # print(fName)
    #print(methods)
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


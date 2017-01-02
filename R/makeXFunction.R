#Make a function of type X (among visualFunction, summaryFunction and
#checkFunction). If classes is null, and fName is a S3 generic,
#the function will make a look-up for all methods available in the
#global enviroment and fill out the slot accordingly.
#Note: This does not work for functions defined wihtin dataMaid. As
#of now, they must have their classes specified explicitly!
#' @importFrom utils methods
makeXFunction <- function(fName, description, classes, X)  {
  if (exists(fName, 1)) {
    f <- get(fName, 1)
  } else {
    f <- get(fName)
  }
   #note: default pos (-1) will look in dataMaid namespace first
   #when called interactively. This results in weird behaviour when
   #users try to overwrite our functions.

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

#tryCatch(mget("identifyMissing", envir = as.environment(1),
#              inherits = T), finally = print("hello"
#                                             )
#         )


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


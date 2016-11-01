
#' @export
makeXFunction <- function(fName, description, classes, X) {
  f <- get(fName)
  if (is.null(classes)) {
    methods <- as.character(methods(fName)) #methods() needs the name in order
    #to work inside the function
    browser()
    print(fName)
    print(methods)
    if (length(methods) > 0) {
      classes <- sub(paste(fName, ".", sep=""),
                     "", methods)
    } else classes <- character()
  }
  class(f) <- c(X, "function")
  attr(f, "description") <- description
  attr(f, "classes") <- classes
  f
}


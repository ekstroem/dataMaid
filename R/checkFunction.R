
#' document me!
#'
#' @param f XXX function
#' @param description XXX
#' @param classes XXX
#' @export
checkFunction <- function(f, description, classes=NULL) {
  f <- deparse(substitute(f))
  makeXFunction(f, description, classes, "checkFunction")
}

#to do: change it such that a checkFunction is constructed e.g. like
# foo <- checkFunction(.description, x) {
#   x + 2
#}


#checkFunction2 <- function(fBody, description = NULL, classes = NULL, message = NULL,
#                           f = NULL) {
#  foo <- function(v, nMax = Inf, maxDecimals = 2) {
#    fBody
#  } #must return list(problem = [boolean], problemValues = [vector or NULL])
#  
#  foo2 <- function(fooList) {
#    outMessage <- messageGenerator(fooList, nMax = nMax, maxDecimals = maxDecimals)
#    checkResult(list(problem = fooList$problem, message = outMessage, 
#                     problemValues = fooList$problemValues))
#  }
#  browser()
#  foo2 <- cleanR:::makeXFunction("foo2", description = description, classes = classes, 
#                        "checkFunction")
#  
#}
#
#dodo <- {#  print("hello")
#}
#fop <- function(dodo) {
#  dodo
#  return(1)
#}


#checkFunction2({list(problem = F, problemValues = NULL)})

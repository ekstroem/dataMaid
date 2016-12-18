#S3 class meant for representing numeric variables that act like 
#factor variables by taking only a few different values. This class
#is used in clean() in order to get appropriate summaries, visualizations
#and checks for such variables. In other words, such variables will be
#treated like factor variables instead of numerics.
smartNum <- function(v) {
  oriClass <- class(v)
  v <- factor(v)
  attr(v, "originalClass") <- oriClass
  class(v) <- c("smartNum", "factor")
  v
}


#Get the original class of a smartNum object. Used in clean().
oClass <- function(v) UseMethod("oClass")
oClass.default <- function(v) class(v)
oClass.smartNum <- function(v) attr(v, "originalClass")

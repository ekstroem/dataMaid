library(xtable)

#Make summaryFunction overview table
sumTab <- data.frame(name=allSummaryFunctions()$name, 
                     description = allSummaryFunctions()$description, 
                     classes = sapply(allSummaryFunctions()$classes, 
                                      function (x) paste(x, collapse=", ")))
sumTab <- as.matrix(sumTab)[,-1]
xtable(sumTab)

#Make visualFunction overview table
visualTab <- data.frame(name=allVisualFunctions()$name, 
                     description = allVisualFunctions()$description, 
                     classes = sapply(allVisualFunctions()$classes, 
                                      function (x) paste(x, collapse=", ")))
visualTab <- as.matrix(visualTab)[,-1]
xtable(visualTab)

#Make checkFunction overview table
checkTab <- data.frame(name=allCheckFunctions()$name, 
                        description = allCheckFunctions()$description, 
                        classes = sapply(allCheckFunctions()$classes, 
                                         function (x) paste(x, collapse=", ")))
checkTab <- as.matrix(checkTab)[,-1]
xtable(checkTab)



toyChecks <- check(toyData)
foo <- function(x) {
  any(sapply(x, function(y) y[["problem"]]))
}
sapply(toyChecks, foo)



#examples
isID <- function(v, nMax = NULL, ...) {
  out <- list(problem = FALSE, message = "")
  if (class(v) %in% c("character", "factor", "labelled", "numeric", "integer")) {
    v <- as.character(v)
    lengths <- nchar(v)
    if (all(lengths > 10) & length(unique(lengths)) == 1) {
      out$problem <- TRUE
      out$message <- "Warning: This variable seems to contain ID codes!"
    }
  }
  out
}


countZeros <- function(v, ...) {
  res <- length(which(v == 0))
  summaryResult(list(feature = "No. zeros", result = res, value = res))
}
countZeros <- summaryFunction(countZeros, description = "Count number of zeros",
                              classes = allClasses())
summarize(toyData, numericSummaries = c(defaultNumericSummaries))



mosaicVisual <- function(v, vnam, doEval) {
  thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}
mosaicVisual <- visualFunction(mosaicVisual, 
                               description = "Mosaic plots using graphics",
                               classes = allClasses())

data(toyData)
pdf("latex/mosaicPlotExample.pdf", 5, 3)
mosaicVisual(toyData$var1, "Variable 1", doEval = TRUE)
dev.off()


identifyColon <- function(v, nMax = Inf, ... ) {
  v <- unique(na.omit(v))
  problemMessage <- "Note: The following values include colons:"
  problem <- FALSE
  problemValues <- NULL
  
  problemValues <- v[sapply(gregexpr("[[:xdigit:]]:[[:xdigit:]]", v),
                            function(x) all(x != -1))]
  
  if (length(problemValues) > 0) {
    problem <- TRUE 
  }
  
  problemStatus <- list(problem = problem, 
                        problemValues = problemValues)
  outMessage <- messageGenerator(problemStatus, problemMessage, nMax)
  
  checkResult(list(problem = problem, 
                   message = outMessage,
                   problemValues = problemValues))
}

identifyColon <- checkFunction(identifyColon, 
                               description = "Identify non-suffixed nor -prefixed colons",
                               classes = c("character", "factor", "labelled"))


clean(toyData, replace = T, 
      preChecks = c("isKey", "isEmpty", "isID"),
      allVisuals = "mosaicVisual",
      characterSummaries = c(defaultCharacterSummaries(), "countZeros"),
      factorSummaries = c(defaultFactorSummaries(), "countZeros"),
      labelledSummaries = c(defaultLabelledSummaries(), "countZeros"),
      numericSummaries = c(defaultNumericSummaries(), "countZeros"),
      integerSummaries = c(defaultIntegerSummaries(), "countZeros"),
      characterChecks = c(defaultCharacterChecks(), "identifyColon"),
      factorChecks = c(defaultFactorChecks(), "identifyColon"),
      lablledCheck = c(defaultLabelledChecks(), "identifyColon"))
      
      
      
      
library(dataMaid)
data(toyData)
toyData
clean(toyData, replace=TRUE)
clean(toyData, output = "html", onlyProblematic = TRUE, maxProbVals = 10,
        replace = TRUE)
clean(toyData, output="html", render=FALSE, openResult=FALSE, replace=TRUE)
defaultFactorSummaries()
clean(toyData, characterSummaries = c("variableType", "centralValue"),
        factorSummaries = c("variableType", "centralValue"),
        labelledSummaries = c("variableType", "centralValue"),
        numericSummaries = c("variableType", "centralValue"),
        integerSummaries = c("variableType", "centralValue"),
        logicalSummaries = c("variableType", "centralValue"),
        dateSummaries = c("variableType", "centralValue"), replace=TRUE)
clean(toyData, allSummaries = c("variableType", "centralValue"), replace=TRUE)
defaultFactorChecks()
clean(toyData, factorChecks = c("identifyWhitespace"), replace=TRUE)
clean(toyData, factorChecks = NULL, replace=TRUE)
clean(toyData, preChecks = "isKey", replace=TRUE)
clean(toyData, allVisuals = "basicVisual", replace=TRUE)
check(toyData$var2)
check(toyData$var2, numericChecks = "identifyMissing")
identifyMissing(toyData$var2)
missVar2 <- identifyMissing(toyData$var2)
str(missVar2)
toyData$var2[toyData$var2 %in% missVar2$problemValues] <- NA
identifyMissing(toyData$var2)
visualize(toyData$var2)
summarize(toyData$var2)
summarize(toyData$var2, numericSummaries = c("centralValue", "minMax"))
allClasses()

isID <- function(v, nMax = NULL, ...) {
  out <- list(problem = FALSE, message = "")
  # Should work for all but logical and Date variables
  if (class(v) %in% setdiff(allClasses(), c("logical", "Date"))) {
    v <- as.character(v)
    lengths <- c(nchar(v))
    # Check that all lengths are the same and greater than 10 characters
    if (all(lengths > 10) & length(unique(lengths)) == 1) {
      out$problem <- TRUE
      out$message <- "Warning: This variable seems to contain ID codes."
    }
  }
  out
}

mosaicVisual <- function(v, vnam, doEval) {
   # Setup the call using the existing function mosaicplot
   thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
   if (doEval) {                     # Return the graphics
    return(eval(thisCall))
   } else return(deparse(thisCall))  # Else return the code for the call
 }

mosaicVisual(toyData$var1, "variable 1", doEval = TRUE)

mosaicVisual <- visualFunction(mosaicVisual,
 			description = "Mosaic plots using graphics",
                               classes = allClasses())

countZeros <- function(v, ...) {
    res <- length(which(v == 0))
    summaryResult(list(feature = "No. zeros", result = res, value = res))
}

countZeros <- summaryFunction(countZeros,
	description = "Count number of zeros",
	classes = c("character", "factor", "integer",
                    "labelled", "numeric"))

identifyColons <- function(v, nMax = Inf, ... ) {
  v <- unique(na.omit(v))
  problemMessage <- "Note: The following values include colons:"
  problem <- FALSE
  problemValues <- NULL

  # Use regular expressions to identify colons between two words
  problemValues <- v[sapply(gregexpr("[[:xdigit:]]:[[:xdigit:]]", v),
                            function(x) all(x != -1))]

  if (length(problemValues) > 0) {
    problem <- TRUE
  }

  problemStatus <- list(problem = problem,
                        problemValues = problemValues)
  # Use the messagegenerator function to produce the output
  # message from the problemStatus and problemMessage
  outMessage <- messageGenerator(problemStatus, problemMessage, nMax)

  checkResult(list(problem = problem,
                   message = outMessage,
                   problemValues = problemValues))
}

identifyColons <- checkFunction(identifyColons,
    description = "Identify colons surrounded by alphanumeric characters",
    classes = c("character", "factor", "labelled"))

data(exampleData)
clean(exampleData,
      # Add the new pre-check function
      preChecks = c("isKey", "isEmpty", "isID"),
      # Change the visual
      allVisuals = "mosaicVisual",
      # Add the new summary for specified data types
      characterSummaries = c(defaultCharacterSummaries(), "countZeros"),
      factorSummaries = c(defaultFactorSummaries(), "countZeros"),
      labelledSummaries = c(defaultLabelledSummaries(), "countZeros"),
      numericSummaries = c(defaultNumericSummaries(), "countZeros"),
      integerSummaries = c(defaultIntegerSummaries(), "countZeros"),
      # Add the new check for specific data types
      characterChecks = c(defaultCharacterChecks(), "identifyColons"),
      factorChecks = c(defaultFactorChecks(), "identifyColons"),
      labelledCheck = c(defaultLabelledChecks(), "identifyColons"), replace=TRUE)
clean(toyData, mode = c("summarize", "check"), replace=TRUE)
clean(toyData, allVisuals = "basicVisual", replace=TRUE)
clean(toyData, render = FALSE, openResult = FALSE, replace=TRUE)
render("dataMaid_toyData.Rmd", quiet = FALSE)
clean(toyData, onlyProblematic = TRUE, mode = c("check"), replace=TRUE)
clean(toyData, onlyProblematic = TRUE, mode = NULL, replace=TRUE)
toyChecks <- check(toyData)
foo <- function(x) {
  any(sapply(x, function(y) y[["problem"]]))
}
sapply(toyChecks, foo)
# clean(toyData, standAlone = FALSE)
# clean(toyData, standAlone = FALSE, output = "html", twoCols = FALSE)

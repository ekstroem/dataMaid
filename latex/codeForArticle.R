library(dataMaid)
library(xtable)

######################################################################################
##################Code examples from the article######################################
######################################################################################

#Section 2
#note: replace = TRUE was added to all clean() calls, compared with how they are 
#presented in the article. This is done because they will produce errors otherwise,
#warning the user about overwriting the file made by the previous clean() calls.
data("toyData")
toyData

clean(toyData, replace = TRUE)

clean(toyData, output = "html", onlyProblematic = TRUE, maxProbVals = 10,
      replace = TRUE)

clean(toyData, output="html", render=FALSE, openResult=FALSE, replace = TRUE) 
  
allSummaryFunctions()

defaultFactorSummaries()

clean(toyData, characterSummaries = c("variableType", "centralValue"),
      factorSummaries = c("variableType", "centralValue"),
      labelledSummaries = c("variableType", "centralValue"),
      numericSummaries = c("variableType", "centralValue"),
      integerSummaries = c("variableType", "centralValue"),
      logicalSummaries = c("variableType", "centralValue"),
      dateSummaries = c("variableType", "centralValue"),
      replace = TRUE)

clean(toyData, allSummaries = c("variableType", "centralValue"), replace = TRUE)

defaultFactorChecks()

clean(toyData, factorChecks = c("identifyWhitespace"), replace = TRUE)

clean(toyData, factorChecks = NULL, replace = TRUE)

clean(toyData, preChecks = "isKey", replace = TRUE)

clean(toyData, allVisuals = "basicVisual", replace = TRUE)


#Section 3
check(toyData$var2)

check(toyData$var2, numericChecks = "identifyMissing")

identifyMissing(toyData$var2)

missVar2 <- identifyMissing(toyData$var2)
str(missVar2) #########OBS: NULL?! - BUG#################

toyData$var2[toyData$var2 %in% missVar2$problemValues] <- NA
identifyMissing(toyData$var2) ###########OBS: Burde vise ingen miscoded missing!#######

visualize(toyData$var2)
summarize(toyData$var2)
summarize(toyData$var2, numericSummaries = c("centralValue", "minMax"))


#Section 4
allClasses()

isID <- function(v, nMax = NULL, ...) {
  out <- list(problem = FALSE, message = "")
  if (class(v) %in% setdiff(allClasses(), c("logical", "Date"))) {
    v <- as.character(v)
    lengths <- c(nchar(v))
    if (all(lengths > 10) & length(unique(lengths)) == 1) {
      out$problem <- TRUE
      out$message <- "Warning: This variable seems to contain ID codes."
    }
  }
  out
}

mosaicVisual <- function(v, vnam, doEval) {
  thisCall <- call("mosaicplot", table(v), main = vnam, xlab = "")
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))  
}

mosaicVisual(toyData$var1, "variable 1", doEval = TRUE)

mosaicVisual <- visualFunction(mosaicVisual,
                               description = "Mosaic plots using graphics",
                               classes = allClasses())

allVisualFunctions()

countZeros <- function(v, ...) {
  res <- length(which(v == 0))
  summaryResult(list(feature = "No. zeros", result = res, value = res))
}

countZeros(c(rep(0, 5), 1:100))

countZeros <- summaryFunction(countZeros,
                              description = "Count number of zeros",
                              classes = c("character", "factor", "integer",
                              "labelled", "numeric"))

identifyColons <- function(v, nMax = Inf, ... ) {
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

identifyColons <- checkFunction(identifyColons,
                                description = "Identify colons surrounded by alphanumeric characters",
                                classes = c("character", "factor", "labelled"))

data("exampleData")
clean(exampleData,
      preChecks = c("isKey", "isEmpty", "isID"),
      allVisuals = "mosaicVisual",
      characterSummaries = c(defaultCharacterSummaries(), "countZeros"),
      factorSummaries = c(defaultFactorSummaries(), "countZeros"),
      labelledSummaries = c(defaultLabelledSummaries(), "countZeros"),
      numericSummaries = c(defaultNumericSummaries(), "countZeros"),
      integerSummaries = c(defaultIntegerSummaries(), "countZeros"),
      characterChecks = c(defaultCharacterChecks(), "identifyColons"),
      factorChecks = c(defaultFactorChecks(), "identifyColons"),
      labelledCheck = c(defaultLabelledChecks(), "identifyColons"))


#Section 5
clean(toyData, mode = c("summarize", "check"))

clean(toyData, allVisuals = "basicVisual")

clean(toyData, render = FALSE, openResult = FALSE)

render("dataMaid_toyData.Rmd", quiet = FALSE)

clean(toyData, onlyProblematic = TRUE, mode = c("check"))

clean(toyData, onlyProblematic = TRUE, mode = NULL)

toyChecks <- check(toyData)

foo <- function(x) {
  any(sapply(x, function(y) y[["problem"]]))
  }

sapply(toyChecks, foo)

clean(toyData, standAlone = FALSE)

clean(toyData, standAlone = FALSE, output = "html", twoCols = FALSE)





###########
makeDataReport(presidentData, replace = TRUE, treatXasY = list(Name = "character"),
               checks = setChecks(character = defaultCharacterChecks(remove = "identifyLoners")),
               reportTitle = "Dirty president data")

presidentData$lastName[presidentData$firstName == "."] 
presidentData$firstName[presidentData$firstName == "."]  <- "Donald"

birthdayOutlierVal <- identifyOutliers(presidentData$birthday)$problemValues
presidentData[presidentData$birthday == birthdayOutlierVal, ]
presidentData <- presidentData[presidentData$birthday != birthdayOutlierVal, ]
summarize(presidentData$presidencyYears)
presidentData[is.na(presidentData$presidencyYears) | 
                     presidentData$presidencyYears %in% 
                    identifyOutliers(presidentData$presidencyYears)$problemValues, 
                   c("firstName", "lastName", "presidencyYears")]





######################################################################################
##################Code used to make tables and figures in the article#################
######################################################################################


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


#########################################################################################################
########DELETE THIS?#####################################################################################
#########################################################################################################
library(microbenchmark)
data(toyData)
microbenchmark(makeDataReport(toyData, replace = TRUE, open = FALSE, quiet = "silent"),
               makeDataReport(toyData, replace = TRUE, mode = c("summarize", "check"), 
                              open = FALSE, quiet = "silent"),
               makeDataReport(toyData, replace = TRUE, 
                              visuals = setVisuals(all = "basicVisual"), open =FALSE, quiet = "silent"),
               times = 10)
#Unit: seconds
#expr
#makeDataReport(toyData, replace = TRUE, open = FALSE)
#makeDataReport(toyData, replace = TRUE, mode = c("summarize",      "check"), open = FALSE)
#makeDataReport(toyData, replace = TRUE, visuals = setVisuals(all = "basicVisual"),      open = FALSE)
#min       lq     mean   median       uq      max neval
#8.218834 23.24083 24.59940 25.06826 27.71859 47.79230    10
#20.093234 21.12455 22.01202 22.26677 22.95272 24.34090    10
#5.416714 20.88525 19.63562 22.73859 24.23288 24.60682    10

data(diamonds)
microbenchmark(makeDataReport(diamonds, replace = TRUE, open = FALSE, quiet = "silent"),
               makeDataReport(diamonds, replace = TRUE, mode = c("summarize", "check"), 
                              open = FALSE, quiet = "silent"),
               makeDataReport(diamonds, replace = TRUE, 
                              visuals = setVisuals(all = "basicVisual"), open =FALSE, quiet = "silent"),
               times = 10)
#Unit: seconds
#expr
#makeDataReport(diamonds, replace = TRUE, open = FALSE, quiet = "silent")
#makeDataReport(diamonds, replace = TRUE, mode = c("summarize",      "check"), open = FALSE, quiet = "silent")
#makeDataReport(diamonds, replace = TRUE, visuals = setVisuals(all = "basicVisual"),      open = FALSE, quiet = "silent")
#min       lq     mean   median       uq      max neval
#23.142882 34.51498 37.40894 37.37907 43.68643 46.23090    10
#7.962945 22.72135 22.51062 24.63406 25.06414 25.74711    10
#24.569014 24.75528 26.01199 25.70402 27.36817 27.94747    10

data(presidentData)
microbenchmark(makeDataReport(presidentData, replace = TRUE, open = FALSE, quiet = "silent"),
               makeDataReport(presidentData, replace = TRUE, mode = c("summarize", "check"), 
                              open = FALSE, quiet = "silent"),
               makeDataReport(presidentData, replace = TRUE, 
                              visuals = setVisuals(all = "basicVisual"), open =FALSE, quiet = "silent"),
               times = 10)
#Unit: seconds
#expr
#makeDataReport(presidentData, replace = TRUE, open = FALSE, quiet = "silent")
#makeDataReport(presidentData, replace = TRUE, mode = c("summarize",      "check"), open = FALSE, quiet = "silent")
#makeDataReport(presidentData, replace = TRUE, visuals = setVisuals(all = "basicVisual"),      open = FALSE, quiet = "silent")
#min       lq     mean   median       uq      max neval
#7.615510 22.73315 25.53273 27.61724 30.88723 37.24841    10
#5.002280 21.32745 20.80463 22.62078 23.46676 24.10571    10
#7.243975 21.77199 23.88189 23.38907 27.30534 39.94023    10

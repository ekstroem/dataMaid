library(dataMaid)

######################################################################################
##################Code examples from the article######################################
######################################################################################

######################################################################################
################################Section 2#############################################
######################################################################################

library("dataMaid")
data("toyData")
toyData


makeDataReport(toyData)


makeDataReport(toyData, output = "html", onlyProblematic = TRUE,
               maxProbVals = 2, replace = TRUE)


makeDataReport(toyData, output="html", render=FALSE,
               openResult=FALSE, replace=TRUE)


allSummaryFunctions()


setSummaries()


defaultCharacterSummaries()


makeDataReport(toyData, replace=TRUE,
               summaries = setSummaries(numeric = c("variableType","centralValue"),
                                        integer = c("variableType", "centralValue")))


makeDataReport(toyData, replace=TRUE,                               
               summaries = setSummaries(all = c("variableType", "centralValue")))


defaultFactorChecks()


makeDataReport(toyData, replace=TRUE,
               checks = setChecks(factor = "identifyWhitespace"))


makeDataReport(toyData, checks = setChecks(factor = NULL), replace=TRUE)



makeDataReport(toyData, preChecks = c("isKey", "isSupported"), 
               replace=TRUE)


makeDataReport(toyData, visuals = setVisuals(all = "basicVisual"), 
               replace=TRUE)


complexData <- data.frame(complexVar = complex(100, real = 1:100, 
                                               imaginary = 3), numericVar = 1:100)
makeDataReport(complexData, treatXasY=list(complex = "numeric"), 
               replace=TRUE)


######################################################################################
################################Section 3#############################################
######################################################################################

check(toyData$events)


check(toyData$events, checks = setChecks(numeric = "identifyMissing"))


identifyMissing(toyData$events)


missEvents <- identifyMissing(toyData$events)
str(missEvents)


toyData$events[toyData$events %in% missEvents$problemValues] <- NA
identifyMissing(toyData$events)


visualize(toyData$events)
summarize(toyData$events)


summarize(toyData$events, 
          summaries = setSummaries(
            numeric = defaultNumericSummaries(remove = c("variableType", 
                                                         "countMissing"))))


######################################################################################
################################Section 4#############################################
######################################################################################

data(presidentData)
head(presidentData)


makeDataReport(presidentData, replace = TRUE, 
               treatXasY = list("Name" = "character"),
               checks = setChecks(character = 
                                    defaultCharacterChecks(remove = "identifyLoners")),
               reportTitle = "Dirty president data")


presidentData$lastName[presidentData$lastName == " Truman"] <- "Truman"
presidentData$stateOfBirth[presidentData$stateOfBirth == "New york"] <- 
  "New York"
presidentData$assassinationAttempt <- 
  factor(presidentData$assassinationAttempt)
presidentData$ageAtInauguration <- 
  as.numeric(presidentData$ageAtInauguration)


presidentData$ageAtInauguration <- 
  as.numeric(as.character(presidentData$ageAtInauguration))


presidentData$lastName[presidentData$firstName == "."]


presidentData$firstName[presidentData$firstName == "."] <- "Donald"


birthdayOutlierVal <- 
  identifyOutliers(presidentData$birthday)$problemValues


presidentData[presidentData$birthday == birthdayOutlierVal, ]


presidentData <- 
  presidentData[presidentData$birthday != birthdayOutlierVal, ]


summarize(presidentData$presidencyYears)


presidentData[is.na(presidentData$presidencyYears) | 
                presidentData$presidencyYears %in% 
                identifyOutliers(presidentData$presidencyYears)$problemValues, 
              c("firstName", "lastName", "presidencyYears")]


presidentData$presidencyYears[presidentData$lastName == "Obama"] <- 8


makeDataReport(presidentData, vol = "_cleaned", 
               treatXasY = list(Name = "character", complex = "numeric"),
               checks = setChecks(character = 
                                    defaultCharacterChecks(remove = "identifyLoners")),
               reportTitle = "Dirty president data - cleaned",
               replace=TRUE)


######################################################################################
################################Section 5#############################################
######################################################################################
#Please note that "replace = TRUE" has been added to a few of the calls below in order
#for them to run in sequence. 

makeDataReport(toyData, open = FALSE, quiet = "silent",
  vol = paste("_", format(Sys.time(), "%m-%d%-%y_%H.%M"), sep = ""))


makeDataReport(toyData, onlyProblematic = TRUE, mode = "check", replace=TRUE)


toyChecks <- check(toyData)
foo <- function(x) {
  any(sapply(x, function(y) y[["problem"]]))
}
sapply(toyChecks, foo)


makeDataReport(toyData, standAlone = FALSE, replace = TRUE)


makeDataReport(toyData, standAlone = FALSE, output = "html", 
  twoCols = FALSE, replace = TRUE)


makeDataReport(toyData, render = FALSE, openResult = FALSE, replace = TRUE)


render("dataMaid_toyData.Rmd", quiet = FALSE)



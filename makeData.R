#generate valid cpr number from birthday ("DDMMYYYY")
makeCPR <- function(bday) {
  day <- as.numeric(strsplit(substring(bday, 1, 2), "")[[1]])
  month <- as.numeric(strsplit(substring(bday, 3, 4), "")[[1]])
  year <- as.numeric(substring(bday, 5, 8))
  mVec <- c(4, 3, 2, 7, 6, 5, 4, 3, 2)
  if (year > 2007) {
    f3d <- c(sample(4:9, 1), sample(0:9, 1), sample(0:9, 1))
    out <- paste(c(day, month, as.numeric(strsplit(substr(year, 3, 4),"")[[1]]), f3d, 1), collapse="")
  }
  if (year <= 2007 & year >= 2000) {
    done <- F
    while(!done) {
      f3d <- c(sample(4:9, 1), sample(0:9, 1), sample(0:9, 1))
      outty <- mVec %*% c(day, month, as.numeric(strsplit(substr(year, 3, 4),"")[[1]]), f3d)
      d10 <- outty %% 11
      if (d10 != 1) {
        out <- paste(c(day, month, as.numeric(strsplit(substr(year, 3, 4),"")[[1]]),
                                              "-", f3d, ifelse(d10==0, 0, 11-d10)), collapse="")
        done <- T
      }
    }
  }
  if (year <= 1999 & year >= 1900) {
    done <- F
    while(!done) {
      f3d <- c(sample(0:3, 1), sample(0:9, 1), sample(0:9, 1))
      outty <- mVec %*% c(day, month, as.numeric(strsplit(substr(year, 3, 4),"")[[1]]), f3d)
      d10 <- outty %% 11
      if (d10 != 1) {
        out <- paste(c(day, month, as.numeric(strsplit(substr(year, 3, 4),"")[[1]]),
                                              "-", f3d, ifelse(d10==0, 0, 11-d10)), collapse="")
        done <- T
      }
    }
  }
  out
}



#make test dataset

library(haven)

vC <- c("a", "b", "c", "a", "b", "d", "a")
vF <- as.factor(vC)
vN <- c(1:10, 1, 1, 1, 5, 5)
vI <- as.integer(vN)
vB <- c(T, F, T, T, T, F)

set.seed(1)
vCPRKey <- sapply(c("01011988", "02011987", "04052006", "01021990", "01021991",
                    "01021993", "01021994", "01021995", "01021996", "01021997",
                    "01021970", "01021971", "01021972", "01021973", "01021974"), makeCPR)
vCPR <- rep(vCPRKey[1:5], 3)

vMiss <- c(".", "", "nan", "NaN", "NAN", "na", "NA", "Na", "Inf", "inf",
           "-Inf", "-inf", "-", "9", "9") #there are more missing strings, add them
#when data is expanded to have more obs.

stringsAsFactorsOption <- getOption("stringsAsFactors")
options(stringsAsFactors = FALSE)
data <- data.frame(charVar=c(rep(vC, 2), NA),
                   factorVar=factor(c(rep(vC, 2), "999")),
                   numVar=vN, intVar=vI, boolVar=c(rep(vB, 2), rep(NA, 3)),
                   keyVar=as.character(1:15),
                   emptyVar=rep(1, 15),
                   numOutlierVar = c(1:14,100),
                   smartNumVar = c(rep(0, 7), rep(1, 8)),
                   cprVar=vCPR,
                   cprKeyVar=vCPRKey,
                   miscodedMissingVar=vMiss,
                   misclassifiedNumVar=factor(c(1:12, 1, 8, 9)),
                   dateVar=as.Date(c("2012-01-01", "2013-04-02", 
                                   rep("1997-05-10", 10),
                                   rep("2005-12-10", 3))),
                   labelledVar = labelled(c(rep(1, 5), rep(2, 8), NA, NA),
                                       labels = c(Peter = 1, Joe = 2)))
options(stringsAsFactors = stringsAsFactorsOption)

testData <- data

save(testData, file="data/testData.rda")
  #Note: .rda files must be written as either .rda or .Rdata (case sensitive!),
  #otherwise, they will not be retrievable using data(), even though they are
  #located in the correct folders.
  #Note also: save() is not case sensitive, so it does not overwrite an identical file,
  #that only differs by the casing of the file extension.


#make smaller dataset for article (toyData)
set.seed(1)
toyData <- data.frame(var1 = c(rep("red", 10), rep("blue", 3), NA, NA),
                      var2 = c(1, 1, 1, 2, 2, 6, 6, 6, 999, NA, 4, 82, NA, NaN, 5),
                      var3 = c(rep("a", 5), rep("b", 3), rep("c", 3), ".", " ", "other", "OTHER"),
                      var4 = rnorm(15),
                      var5 = as.character(1:15),
                      var6 = rep("Irrelevant", 15))
                      
save(toyData, file="data/toyData.rda")



########Make US president data
#Based on csv dataset from http://www.data-explorer.com/data
library(haven)
library(lubridate)
presidentData <- read.csv("USPresidents.csv", na.strings = "",
                          stringsAsFactors = FALSE)
presidentData[, "Image.Path"] <- NULL
presidentData[, "ID"] <- NULL
presidentData[, "Town.or.County.of.Birth"] <- NULL
presidentData[, "Home.State"] <- NULL 
presidentData[, "Assassinated"] <- NULL
presidentData[, "Religious.Affiliation"] <- NULL
presidentData[, "Middle.Name"] <- NULL
names(presidentData) <- c("lastName", "firstName", "orderOfPresidency", 
                          "birthday", "dateOfDeath", "stateOfBirth",
                          "party", "presidencyBeginDate", "presidencyEndDate",
                          "assassinationAttempt")
presidentData$sex <- "Male"
presidentData$ethnicity <- "Caucasian"
presidentData[presidentData$lastName == "Obama", "ethnicity"] <-  "African American"
presidentData[presidentData$lastName == "Obama", "presidencyEndDate"] <-  "1/20/2017"
presidentData <- rbind(presidentData, data.frame(lastName = c("Trump", "Arathornson"), 
                                                 firstName = c(".", "Aragorn"), 
                                                 orderOfPresidency = c(45, 0), 
                                                 birthday = c("6/14/1946", "3/1/1300"),
                                                 dateOfDeath = c(NA, "1/1/1510"), 
                                                 stateOfBirth = c("New york", "Gondor"),
                                                 party = c("Republican", "Dunedain"), 
                                                 presidencyBeginDate = c("1/20/2017", NA),
                                                 presidencyEndDate = c(NA, NA),
                                                 assassinationAttempt = c("false", "true"),
                                                 sex = c("Male", "Male"), 
                                                 ethnicity = c("Caucasian", "Caucasian")))
presidentData$birthday <- as.Date(presidentData$birthday, format = "%m/%d/%Y")
presidentData$dateOfDeath <- as.Date(presidentData$dateOfDeath, format = "%m/%d/%Y")
presidentData$presidencyBeginDate <- as.Date(presidentData$presidencyBeginDate, format = "%m/%d/%Y")
presidentData$presidencyEndDate <- as.Date(presidentData$presidencyEndDate, format = "%m/%d/%Y")
presidentData$precidencyYears <- round(as.numeric(presidentData$presidencyEndDate - 
                                               presidentData$presidencyBeginDate)/365.25,0)
presidentData$precidencyYears[presidentData$lastName == "Obama"] <- Inf
presidentData$ageAtInauguration <- round(as.numeric(presidentData$presidencyBeginDate - 
                                                presidentData$birthday)/365.25,0)
presidentData$orderOfPresidency <- as.factor(presidentData$orderOfPresidency)
class(presidentData$lastName) <- class(presidentData$firstName) <- "Name"
presidentData$dayOfDeath <- NULL
presidentData$party <- NULL
presidentData$assassinationAttempt <- as.numeric(factor(presidentData$assassinationAttempt))-1
set.seed(1)
presidentData$favoriteNumber <- sample(1:10, 46, replace = TRUE)
presidentData$favoriteNumber[presidentData$lastName == "Jefferson"] <- sqrt(as.complex(-2))
presidentData$sex <- factor(presidentData$sex)
presidentData$ethnicity <- factor(presidentData$ethnicity)
presidentData$ageAtInauguration <- as.character(presidentData$ageAtInauguration)
presidentData$presidencyBeginDate <- NULL
presidentData$presidencyEndDate <- NULL
presidentData$dateOfDeath <- NULL
presidentData$lastName[presidentData$lastName == "Truman"] <- " Truman"

save(presidentData, file="data/presidentData.rda")





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
                   joeVar = 1:15,
                   jack__var = 1:15,
                   numOutlierVar = c(1:14,100),
                   smartNumVar = c(rep(0, 7), rep(1, 8)),
                   cprVar=vCPR,
                   cprKeyVar=vCPRKey,
                   miscodedMissingVar=vMiss,
                   misclassifiedNumVar=factor(1:15))
names(data)[names(data)=="joeVar"] <- "_joeVar"
options(stringsAsFactors = stringsAsFactorsOption)

testData <- data

save(testData, file="data/testData.Rda")

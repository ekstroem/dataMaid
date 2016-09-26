#make test dataset

vC <- c("a", "b", "c", "a", "b", "d", "a")
vF <- as.factor(vC)
vN <- c(1:10, 1, 1, 1, 5, 5)
vI <- as.integer(vN)
vB <- c(T, F, T, T, T, F) 

vCPR <- rep(c("010101-1111", "020102-2929", "121201-1902", "030729-2222",
              "080909-1212"), 3)

vCPRkey <- c(vCPR[1:5], "020202-0101", "030303-0101", "040404-0101", "050505-0101",
             "060606-0101", "070707-0101", "080808-0101", "090909-0101", 
             "020202-0202", "030303-0202")
vMiss <- c(".", "", "nan", "NaN", "NAN", "na", "NA", "Na", "Inf", "inf",
             "-Inf", "-inf", "-", "9", "9") #there are more missing strings, add them 
                                            #when data is expanded to have more obs.

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
                   cprKeyVar=vCPRkey,
                   miscodedMissingVar=vMiss)
names(data)[names(data)=="joeVar"] <- "_joeVar"

testData <- data

save(testData, file="testData.Rda")

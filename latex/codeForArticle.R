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
#Aggregates data ready for histogram plotting via ggplot2 and
#geom_rect. This means that plotting code length depends
#on the number of bins, not on the length of v.
#Uses binning procedure from hist() so that basicVisual and 
#standardVisual create identical histograms, except for 
#plotting style. 
#note: "Sturges" is breaks default in hist()
#and we need to set breaks for Date variables only
aggregateForHistogram <- function(v, breaks = "Sturges") {
  d <- hist(v, plot = FALSE, breaks = breaks)
  nBins <- length(d$counts)
  out <- data.frame(xmin = d$breaks[-(nBins+1)],
                    xmax = d$breaks[-1],
                    ymin = 0,
                    ymax = d$counts)
  out
}

#testing 
#a <- rexp(100000)
#d <- aggregateForHistogram(a)
#ggplot(d, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
# geom_rect(col = "white")
#p2 <- ggplot(data.frame(a = a), aes(x = a)) +
#  geom_histogram(col = "white", breaks = cuts) #note: cuts should be set globally for this line to work
#grid.arrange(p1, p2, nrow = 2)


#Old version
#aggregateForHistogram <- function(v, bins = 20) {
#  minVal <- min(v)
#  maxVal <- max(v)
#  binWidth <- (maxVal - minVal)/bins 
#  if (binWidth != 0) {
#    cuts <- c(minVal, (minVal + (1:(bins-1))*binWidth), maxVal)
#    factorV <- cut(v, breaks = cuts, include.lowest = TRUE)
#    outF <- as.data.frame(table(factorV))
#    outF$xmin <- cuts[-(bins+1)]
#    outF$xmax <- cuts[-1]
#    outF$ymin <- 0
#    outF$ymax <- outF$Freq
#  } else {
#    outF <- data.frame(xmin = minVal, xmax = maxVal, 
#                       ymin = 0, ymax = length(v))
#  }
#  outF
#}
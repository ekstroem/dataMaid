##########################################Not exported below#########################################


########fancyVisual###########

#NOT DONE - SHOULD NOT BE EXPORTED. NEED TO IMPLIMENT DOEVAL=F STUFF FIRST...

#THINK ABOUT: use myDistrPlots-ish function from my thesis?
#       - adds density smoothers to histograms
#       - for barplots: move x-axis breaks into the plot for more
#           control wrt. the (actual) size of the plot

#TO DO: If we are to keep "fancy"-option: Rewrite myDistrPlots
#       in s3 style and such that it is only designed to produce one
#       plot

#NOTE: the use of substitute() requires that v is a named vector.
#       better way to do this? And will this be a problem in our
#       setting? I don't think so.

fancyVisual <- function(v, vnam) {
  data <- data.frame(v)
  names(data) <- vnam
  myDistrPlots(data, vnam)[[1]]
}


#' @importFrom ggplot2 ggplot aes aes_string geom_bar geom_text scale_fill_discrete scale_x_continuous xlab ylab ggtitle theme_bw geom_histogram geom_line scale_x_discrete
myDistrPlots <- function(data, var, labels=NULL,
                         makeCont=NULL,
                         makeCat=NULL,
                         catStyle="bars",
                         adjustFunction=function(x){1}) {
  if (is.null(labels)) labels <- var
  n <- length(var)
  outLst <- NULL
  if (class(data[, var]) %in% c("logical", "character", "labelled")) {
    data[, var] <- factor(data[, var])
  }
  for (i in 1:n) {
    thisVar <- var[i]
    if ((is.factor(data[, thisVar]) & !(thisVar %in% makeCont)) |
        thisVar %in% makeCat) {
      if (thisVar %in% makeCat) {
        data[, thisVar] <- factor(data[, thisVar])
      }
      thisTab <- table(data[, thisVar])
      thisCount <- as.numeric(c(thisTab))
      tpDat <- data.frame(freq=thisCount/sum(thisCount),
                          lab=factor(names(thisTab), levels=levels(data[, thisVar])))
      if (catStyle == "stacked") {
        p <- ggplot(tpDat, aes(x=1, y=freq, fill=lab,
                               label=lab)) +
          geom_bar(stat="identity") +
          geom_text(position="stack", vjust=1) +
          scale_fill_discrete(breaks=NULL) +
          scale_x_continuous(breaks=NULL) +
          xlab("") +
          ylab("Cumulative frequency") +
          ggtitle(labels[i]) +
          theme_bw()
      } else {
        p <- ggplot(tpDat, aes(x=lab, y=freq, fill=lab,
                               label=lab)) +
          geom_bar(stat="identity") +
          geom_text(angle=90, aes(y=0),
                    hjust=-0.01, size=3) +
          scale_x_discrete("", breaks=NULL) +
          ylab("Frequency") +
          scale_fill_discrete(breaks=NULL)+
          ggtitle(labels[i]) +
          theme_bw()

      }
    } else {
      if (thisVar %in% makeCont) {
        data[, thisVar] <- as.numeric(as.character(data[, thisVar]))
      }
      p <- ggplot(data, aes_string(x=thisVar)) +
        geom_histogram(mapping=aes(y=..density..),
                       col="white", fill="#00BFC4",
                       bins=20) +
        geom_line(size=1, col="black", stat="density",
                  adjust=adjustFunction(data[, thisVar])) +
        xlab("") +
        ylab("Density") +
        ggtitle(labels[i]) +
        theme_bw()
    }
    outLst <- c(outLst, list(p))
  }
  outLst
}

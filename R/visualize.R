library(ggplot2)

#QUESTION: Simpler way to parse allVisual = "standardVisual"-argument
#           on to every method?



#Make plot of a variable v, depending on its data type
#plotting function is supplied in the argument allVisual 
#(for all data types) or separately in the arguments
#characterVisual, factorVisual etc. for each datatype. 
#Note that data type specific arguments (e.g. characterVisual)
#overwrites allVisual, if supplied.
#NA, NaN and Inf values are ignored for numeric/integer 
#variables. only NA are ignored for character/factor/labelled/logical
#variables. 


#NOTE: smartNum option should be removed(?) - it is done 
#using a smartNum s3 class now. 

visualize <- function(v, vnam, allVisual = "standardVisual", 
                      doEval=T, ...) UseMethod("visualize")

visualize.character <- function(v, vnam, allVisual = "standardVisual", 
                                characterVisual=NULL, doEval=T, ...) {
  useVisual <- ifelse(is.null(characterVisual), allVisual, characterVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.factor <- function(v, vnam, allVisual = "standardVisual", 
                             factorVisual = NULL, doEval=T, ...) {
  useVisual <- ifelse(is.null(factorVisual), allVisual, factorVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.labelled <- function(v, vnam, allVisual = "standardVisual",
                               labelledVisual = NULL, doEval=T, ...) {
  useVisual <- ifelse(is.null(labelledVisual), allVisual, labelledVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.numeric <- function(v, vnam, allVisual = "standardVisual", 
                              numericVisual = NULL, smartNum = F, 
                              doEval = T, ...) {
  useVisual <- ifelse(is.null(numericVisual), allVisual, numericVisual)
  eval(call(useVisual, v, vnam, doEval=doEval))
}

visualize.integer <- function(v, vnam, allVisual = "standardVisual", 
                              integerVisual = NULL, smartNum = F, 
                              doEval = T, ...) {
  useVisual <- ifelse(is.null(integerVisual), allVisual, integerVisual)
  eval(call(useVisual, v, vnam, doEval))
}

visualize.logical <- function(v, vnam, allVisual = "standardVisual",
                              logicalVisual = NULL, doEval = T, ...) {
  useVisual <- ifelse(is.null(logicalVisual), allVisual, logicalVisual)
  eval(call(useVisual, v, vnam, doEval))
}


#NOTE: no need for a smartNum visual - it is caught by "factor"
#visualize.smartNum <- function(v, vnam, allVisual = "standardVisual",
#                               smartNumVisual = NULL, doEval = T) {
#  
#}



#############################################################


########standardVisual###########

standardVisual <- function(v, vnam, doEval=T, ...) UseMethod("standardVisual") 

#Makes "standard" (ggplot2) visualization of a variable v,
#i.e. barplots for qualitative variables and histograms
#for quantitative variables. 
#if smartNum=T, the function checks whether a numeric/integer
#seems to be categorical (i.e. only has <=3 levels), and in that 
#case, it is plotted using a barplot.

#character, factor, labelled and logical variables
standardVisualCFLB <- function(v, vnam, doEval=T, ...) {
  thisCall <- call("qplot", x=na.omit(v), geom="bar", xlab="", main=vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#numeric and integer variables
standardVisualIN <- function(v, vnam, doEval=T, smartNum=T) {
  v <- v[is.finite(v)]
  if (smartNum) {
    if (length(unique(v)) <= 3) {
      return(standardVisualCFLB(as.factor(v), vnam, doEval))
    }
  } 
  thisCall <- call("qplot", x=na.omit(v), geom="histogram", xlab="",
                    main=vnam, bins=20)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
} #fix such that no stat_bin()-message is produced, it's annoying. And also a clever
  #choice of binwidth, how does it work in hist()?


#assign methods to generic standardVisual function
standardVisual.character <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)
standardVisual.factor <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)
standardVisual.labelled <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)
standardVisual.numeric <- function(v, vnam, doEval=T, smartNum=T) standardVisualIN(v, vnam, 
                                                                                   doEval=doEval, 
                                                                                   smartNum=smartNum)
standardVisual.integer <- function(v, vnam, doEval=T, smartNum=T) standardVisualIN(v, vnam, 
                                                                                   doEval=doEval, 
                                                                                   smartNum=smartNum)
standardVisual.logical <- function(v, vnam, doEval=T) standardVisualCFLB(v, vnam, doEval=doEval)


#####################################################################


########basicVisual###########

basicVisual <- function(v, vnam, doEval = T, ...) UseMethod("basicVisual") 

#Makes plots using the standard R plotting options

#character, factor, labelled and logical variables
basicVisualCFLB <- function(v, vnam, doEval=T, ...) {
  v <- as.factor(v)
  thisCall <- call("plot", x=na.omit(v), main=vnam)
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
}

#numeric and integer variables
basicVisualIN <- function(v, vnam, doEval=T) {
  v <- v[is.finite(v)]
  thisCall <- call("hist", v, main=vnam, col="grey", xlab="")
  if (doEval) {
    return(eval(thisCall))
  } else return(deparse(thisCall))
} 

#assign methods to generic standardVisual function
basicVisual.character <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)
basicVisual.factor <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)
basicVisual.labelled <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)
basicVisual.numeric <- function(v, vnam, doEval=T) basicVisualIN(v, vnam, doEval=doEval)
basicVisual.integer <- function(v, vnam, doEval=T) basicVisualIN(v, vnam, doEval=doEval)
basicVisual.logical <- function(v, vnam, doEval=T) basicVisualCFLB(v, vnam, doEval=doEval)



#####################################################################


########fancyVisual###########

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
          ylab("Cummulative frequency") +
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
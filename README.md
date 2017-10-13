# dataMaid

dataMaid is an R package for documenting and creating reports on data cleanliness. 


## Installation

This github page contains the *development version* of dataMaid. For the
latest stable version download the package from CRAN directly using

```{r}
install.packages("dataMaid")
```

To install the development version of dataMaid run the following
commands from within R (requires that the `devtools` package is already installed)

```{r}
devtools::install_github('ekstroem/dataMaid')
```

![Download counter](http://cranlogs.r-pkg.org/badges/grand-total/dataMaid)


## Package overview

A super simple way to get started is to load the package and use the
`makeDataReport` function on a data frame (if you try to generate several
reports for the same data, then it may be necessary to add the `replace=TRUE`
argument to overwrite the existing report). 

```{r}
library(dataMaid)
data(trees)
makeDataReport(trees)
```

This will create a report with summaries and error checks for each
variable in the `trees` data frame. Note that the standard settings of 
`makeDataReport()` will make a pdf report. For this to work, you
need to have [LaTeX](https://www.latex-project.org/) installed on your machine. 
If you don't want to install LaTex, you can produce a report in Word or html formats
by using the `output` argument:
```{r}
makeDataReport(trees, output = "word", replace = TRUE)
```


### Using dataMaid interactively

The dataMaid package can also be used interactively by running checks for the individual variables or for all variables in the dataset

```{r}
data(toyData)
check(toyData$var2)  # Individual check of var2
check(toyData) # Check all variables at once
```

By default the standard battery of tests is run depending on the
variable type. If we just want a specific test for, say, a numeric
variable then we can specify
that. All available checks can be viewed by calling `allCheckFunctions()`. See
[the documentation](https://github.com/ekstroem/dataMaid/blob/master/latex/article_vol2.pdf) for
an overview of the checks available or how to create and include your own tests.


```{r}
check(toyData$var2, numericChecks = "identifyMissing")
```

We can also access the graphics or summary tables that are produced for a variable by calling the `visualize` or `summarize` functions

```{r}
visualize(toyData$var2)
summarize(toyData$var2)  # All summaries
summarize(toyData$var2, summaries = setSummaries(numeric =  c("centralValue", "minMax"))  ## Only two numeric summaries
```


## Detailed documentation

[This manuscript](https://github.com/ekstroem/dataMaid/blob/master/latex/article_vol2.pdf) draft
provides a detailed introduction to the dataMaid package. At one point
it will be added as a vignette.




## Online app

We are currently working on an online version of the tool, where users
can upload their data and get a report. A prototype
is already up and running - we just need to configure the R server correctly.

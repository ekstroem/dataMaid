context("dataMaid summarize")

library(dataMaid)
                                       
## Define atomic types
typel <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
typei <- c(1L, 2L, 3L, 4L, 5L)
typed <- c(1.0, 2.3, 4.5, 6.7, 8.9)
typec <- c(1 + 2i, 2 + 0i, 3 + 3i, 4 - 0i, 5+2i)
types <- LETTERS[1:5]
typer <- sapply(types, charToRaw)
typelist <- list(a=1:3, b=1:10)

##
## Check that the right check are performed for each atomic type
##

## Each type should result in a list
test_that("summarize returns a list for (most) atomic vectors", {
    expect_is(summarize(typel), "list")
    expect_is(summarize(typei), "list")
    expect_is(summarize(typed), "list")
    expect_error(summarize(typec))
    expect_is(summarize(types), "list")
    expect_error(summarize(typelist))
    expect_error(summarize(typer))    
})

## Summarize the right number of tests. This needs to be updated 
test_that("summarize return the right number of tests for (most) atomic vectors", {
    expect_equal(length(summarize(typel)), 4)
    expect_equal(length(summarize(typei)), 6)
    expect_equal(length(summarize(typed)), 6)
    ##expect_is(summarize(typec), "list")
    expect_equal(length(summarize(types)), 4)
    ##expect_is(summarize(typer), "list")
})  

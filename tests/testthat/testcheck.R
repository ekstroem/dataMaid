context("dataMaid check")

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
test_that("check returns a list for (most) atomic vectors", {
    expect_is(check(typel), "list")
    expect_is(check(typei), "list")
    expect_is(check(typed), "list")
    expect_error(check(typec))
    expect_is(check(types), "list")
    expect_error(check(typelist))
    expect_error(check(typer))    
})

## Check the right number of tests. This needs to be updated 
test_that("check return the right number of tests for (most) atomic vectors", {
    expect_equal(length(check(typel)), 1)
    expect_equal(length(check(typei)), 2)
    expect_equal(length(check(typed)), 2)
    ##expect_is(check(typec), "list")
    expect_equal(length(check(types)), 5)
    ##expect_is(check(typer), "list")
})  

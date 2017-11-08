context("summary function: minMax")

library(dataMaid)


## Read atomic vectors
source("atomic.R")
                                       
## Each type should result in a list
test_that("minMax returns a summaryResult", {

    ## First check the atomic vectors
    expect_equal(minMax(typel)$value[1], 0L)
    expect_equal(minMax(typel)$value[2], 1L)
    expect_equal(minMax(typei)$value[1], 1L)
    expect_equal(minMax(typei)$value[2], 5L)
    expect_equal(minMax(typed)$value[1], 1.0)
    expect_equal(minMax(typed)$value[2], 8.9)

    expect_error(minMax(typec))
    expect_error(minMax(types))
    expect_error(minMax(typef))
    expect_error(minMax(typer))
    expect_error(minMax(typelab))
    expect_error(minMax(typelist))
    expect_equal(minMax(typeDate)$value[1], as.Date("1960-01-01"))


    ## Check the output format    
    expect_is(minMax(1:5), "summaryResult")
    expect_is(minMax(c(NA, 1:5)), "summaryResult")    
    expect_is(minMax(c(NA, NA, NA)), "summaryResult")
    expect_is(minMax(c(NA, NA, Inf)), "summaryResult")
    
    expect_equal(minMax(1:5)$value[1], 1)
    expect_equal(minMax(c(NA, 1:5))$value[1], 1)
    expect_equal(minMax(c(NA, NA, NA))$value[1], NA)
    expect_equal(minMax(c(NA, NA, Inf))$value[1], Inf)

})


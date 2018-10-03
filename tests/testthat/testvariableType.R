context("summary function: variableType")

library(dataMaid)


## Read atomic vectors
source("atomic.R")


test_that("variableType returns the right value", {
    ## First check the atomic vectors
    expect_equal(variableType(typel)$value, "logical")
    expect_equal(variableType(typei)$value, "integer")
    expect_equal(variableType(typed)$value, "numeric")
    expect_equal(variableType(typec)$value, "complex")
    expect_equal(variableType(types)$value, "character")
    expect_equal(variableType(typef)$value, "factor")
    expect_true(variableType(typelab)$value %in% c("labelled", "haven_labelled"))
    expect_equal(variableType(typelist)$value, "list")
    expect_equal(variableType(typeDate)$value, "Date")
})



## Each type should result in a list
test_that("variableType returns a summaryResult", {

    ## Check the output format    
    expect_is(variableType(typel), "summaryResult")
    expect_is(variableType(typei), "summaryResult")
    expect_is(variableType(typed), "summaryResult")
    expect_is(variableType(typec), "summaryResult")
    expect_is(variableType(types), "summaryResult")
    expect_is(variableType(typef), "summaryResult")
    expect_is(variableType(typelab), "summaryResult")
    expect_is(variableType(typelist), "summaryResult")
    expect_is(variableType(typeDate), "summaryResult")

})


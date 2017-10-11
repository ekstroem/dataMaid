context("dataMaid minMax")

library(dataMaid)
                                       
## Each type should result in a list
test_that("minMax returns a summaryResult", {
    expect_is(minMax(1:5), "summaryResult")
    expect_is(minMax(c(NA, 1:5)), "summaryResult")    
    expect_is(minMax(c(NA, NA, NA)), "summaryResult")
    expect_is(minMax(c(NA, NA, Inf)), "summaryResult")
    
    expect_equal(minMax(1:5)$value[1], 1)
    expect_equal(minMax(c(NA, 1:5))$value[1], 1)
    expect_equal(minMax(c(NA, NA, NA))$value[1], NA)
    expect_equal(minMax(c(NA, NA, Inf))$value[1], Inf)


})


context("Testing input/output of makeDataReport")

# Test for labeled

# Read a dummy SPSS dataset
load("spss_labelled.rda")

test_that("Can parse an SPSS dataset with labels", {

    expect_null(makeDataReport(dummydata, render=FALSE, replace=TRUE, openResult=FALSE))

})


# Remove the mess we've made so far

unlink("dataMaid_dummydata.Rmd") 
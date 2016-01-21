library(transprob)
context("Special cases")

test_that("labels that don't appear in the data don't affect the answer", {
  expect_equal(  # check late date doesn't remove any rows
    transprob(Embankment, labels=c('A','B','C','D','E', 'F'), end_date = '28-Feb-2007',removeSingleton = FALSE, ObservedOnly = FALSE, transInterval = 1)[1:5,1:5],
    transprob(Embankment, labels=c('A','B','C','D','E'), end_date = '28-Feb-2007',removeSingleton = FALSE, ObservedOnly = FALSE, transInterval = 1))})
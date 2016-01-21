library(transprob)
context("Format data")

test_that("inspections that occur after end_date are removed", {
  expect_equal(
    dim(FormatData(FinancialRating, 
                   c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D'), end_date='20-Nov-2015'))[1], 4315) 
  # check late date doesn't remove any rows
  expect_equal( # check removes expected number
    dim(FormatData(FinancialRating, 
                   c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D'), end_date='19-Aug-1982'))[1], 1)
  
  expect_equal( # check removes expected number
    dim(FormatData(FinancialRating, 
                   c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D'), end_date='19-Aug-1982'))[1], 1)
})

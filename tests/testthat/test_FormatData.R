library(transprob)
context("Format data")

test_that("inspections that occur after end_date are removed", {
  expect_equal(  # check late date doesn't remove any rows
    dim(FormatData(FinancialRating, 
                   c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D'), end_date='20-Nov-2015'))[1], 4315) 

  expect_equal( # check removes expected number
    dim(FormatData(FinancialRating, 
                   c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D'), end_date='18-Aug-1982'))[1], 1)
  
  expect_equal( # check date before start removes all the data
    dim(FormatData(FinancialRating, 
                   c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D'), end_date='10-Aug-1982'))[1], 0)
  
  expect_equal( # check that no end date defaults to including all the data
    dim(FormatData(FinancialRating, 
                   c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D')))[1], 4315)
  
  })

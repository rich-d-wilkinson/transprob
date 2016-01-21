library(transprob)
context("bootstrapping")

test_that("bootPrepData performs the correct formatting", {
  
  data = data.frame(ID=c(1,1,1,2,3,3,4), Date = Embankment$Date[1:7], EHC=Embankment$EHC[1:7])
  expect_equal( 
    bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = FALSE, ObservedOnly = FALSE)$startrow,
       c(1,4,5,7))
  
  expect_equal( 
    bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = FALSE, ObservedOnly = FALSE)$endrow,
    c(3,4,6,7))
  
  expect_equal( 
    as.numeric(bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = FALSE, ObservedOnly = FALSE)$IDs),
    c(1,2,3,4))
  # as.numeric needed as IDs is a factor by default
  expect_equal( 
    as.numeric(bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = TRUE, ObservedOnly = FALSE)$IDs),
    c(1,3))
  
  expect_equal( 
    bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = TRUE, ObservedOnly = FALSE)$startrow,
    c(1,4)) ## note 1,4 not 1,5 as the data startrow refers to is the dataset with singletons removed

  expect_equal( 
    bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = TRUE, ObservedOnly = FALSE)$endrow,
    c(3,5))
  
})


test_that("bootsample samples correct number of assets",{
  bootFdata = bootPrepData(Embankment[1:100,], labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = FALSE, ObservedOnly = FALSE)
  nasset =  length(table(bootFdata$IDs))
  out = bootsample(bootFdata)
  expect_equal(length(table(out$bootData$ID)), nasset)
  
  expect_equal(length(out$bootIDs), nasset)
})


test_that('bootstrap deals with end_date correctly',{
  
  data = data.frame(ID=c(1,1,1,2,3,3,4), Date = Embankment$Date[1:7], EHC=Embankment$EHC[1:7])
  end_date = "17-Mar-2011"
  bootFdata = transprob:::bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), end_date=end_date, removeSingleton = FALSE, ObservedOnly = FALSE)
  
  expect_equal( # if not, then the following tests will fail
    dim(bootFdata$Fdata)[1], 3)
})


test_that("bootsample works correctly", {
  data = data.frame(ID=c(1,1,1,2,3,3,4), Date = Embankment$Date[1:7], EHC=Embankment$EHC[1:7])
  bootFdata = bootPrepData(data, labels=c('A', 'B', 'C', 'D', 'E'), removeSingleton = FALSE, ObservedOnly = FALSE)
  set.seed(1)
  out = bootsample(bootFdata)
  
  expect_equal( # if not, then the following tests will fail
    out$bootIDs, c(2,2,3,4))

  expect_equal( # check new IDs work
    out$bootData$ID, c(1,2,3,3,4)
  )
  expect_equal(# check correct rows chosen
    out$bootRows, c(4,4,5,6,7)
  )
  
  for(i in 1:5){
    expect_equal(# check correct bootstrap sample
     out$bootData$Date[i], data$Date[out$bootRows[i]]#factor(c(data$Date[4], data$Date[4], data$Date[5], data$Date[6], data$Date[7])) 
    )}
  })
  

  
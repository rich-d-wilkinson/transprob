

#' bootstrap replicates of the degradation matrix
#'
#' This function randomly samples assets from the original data (keeping the same number of assets, which may result in a 
#' different number of records), and calculates the degradation matrix. It repeats this nboot times
#' 
#' @param data   A data frame consisting of at least three columns. The first column must be the asset ID,
#'  the second column is the inspection date, and the third column is the rating. The inspections must be in chronological order for each asset
#'
#' @param labels   A vector listing all the different asset ratings that occur. Not every rating in labels must occur in the data,
#' but there can't be ratings in the data that don't occur in labels. Note that the order specified in labels determines the ordering of
#' the rows and columns in the transition matrix.
#' @param date_format  A string (i.e. in quotations) giving the format of the date column of data.
#' The format descriptions allowed are described at http://www.r-bloggers.com/date-formats-in-r/
#' @param end_date = If ObservedOnly=FALSE, then the calculation assumes that the asset rating does not change from the last observation
#' time. The total amount of time used in the calculation is determined by end_date. I.e., if the last inspection was 2012 and end_date was 2015, then this would add 3 years tothe total.
#' If not value is specified, then the date of the last observation is used.
#' @param removeSingleton  a logical value  indicating whether to remove assets that only have a single observation recorded
#' or not. If FALSE, then assets with only a single observation are assumed to stay in that state through to end_date
#' @param transInterval length of transition interval considered in years. Defaults to 1 year.
#' @param ObservedOnly a logical value indicating whether to assume that assets remained in their current condition from the
#' last inspection date until end_date, or whether just to count the time between observations.

#' @param nboot the number of bootstrap replicates
#'
#' @return A list of nboot degradation matrix estimates. These are all estimates that are consistent with the data.
#' 
#' @export
bootstrap <- function(data, labels, date_format="%d-%b-%Y", end_date=NULL, 
                      removeSingleton=TRUE, transInterval=1, ObservedOnly=FALSE, nboot=10){
  
  
  bootFdata <- bootPrepData(data, labels, date_format=date_format, end_date=end_date, 
                          removeSingleton=removeSingleton, transInterval=transInterval, ObservedOnly=ObservedOnly)
  
  
  if(!is.null(end_date)) {
    end_date = strptime(end_date, date_format) # put date in correct form
  } else {
    end_date = max(bootFdata$Fdata$Std_dates)
  }

  
  out <- list()
  for(nn in 1: nboot){
      #print(nn)
      bootReplicate <- bootsample(bootFdata)$bootData # generate bootstrap dataset
      
      Trans <- CalcTrans(bootReplicate,  labels, Cpp=TRUE)
      Times = CalcTime(bootReplicate,  labels=labels, end_date, ObservedOnly, Cpp=TRUE)
      Q = CalcQ(Trans,Times)
      out[[nn]] <- expm::expm(Q*transInterval)
  }
  return(out)
}


bootsampleSLOW <- function(Fdata, IDs){ ## Too slow
  bootIDs <- sample(IDs, replace=TRUE)
  tmp <- lapply(bootIDs, function(x) ExtractAsset(Fdata, x))
  do.call(rbind, tmp)
}

#` Internal function to generate the information needed in order to do bootstrapping
#`
#`
bootPrepData <- function(data, labels, date_format="%d-%b-%Y", end_date=NULL, 
                         removeSingleton=TRUE, transInterval=1, ObservedOnly=FALSE){

  Fdata = FormatData(data=data, labels=labels, date_format, end_date=end_date)
  IDtable <- as.data.frame(table(Fdata[,'ID']))
# Remove Singletons
  if(removeSingleton){
  
    nonSingletonID <- IDtable[IDtable[,2]>1,]
    Fdata <- Fdata[Fdata[,1] %in% nonSingletonID[,1],]
    IDs = nonSingletonID[,1]
  } else IDs = IDtable[,1]
### end of same as transprob

# a list of the rows in the data
  rowindex <- 1:length(Fdata$ID)

  diffs <- as.logical(diff(Fdata$ID, lag=1))  # is a row ID the same as the next row ID: if so diff will be zero,   
# as.logical turns 0 to FALSE and non-zero to TRUE


### the row in Fdata in which each asset entry ends
  endrow <-rowindex[c(diffs, TRUE)] # add extra TRUE on end as diffs is 1 element short and 
# we want to include the last row of Fdata, as it is the last row of that asset.

### the row in Fdata in which each asset entry begins
  startrow <- rowindex[c(TRUE, diffs) ] ## add extra TRUE on start for similar reason to the above
  
  return(list(Fdata=Fdata, startrow=startrow, endrow=endrow, IDs = IDs, diffs=diffs))
}


#' An internal function to carry out the bootstrap sampling
bootsample <- function(bootFdata){
  
  bootIDs <- sample(1:length(bootFdata$IDs), replace = TRUE) # sample a set of IDs, with same number of assets as before, with replacement
  # create the list of the rows in Fdata to extract corresponding to bootIDs
  
  bootRows <- unlist(apply(cbind(bootFdata$startrow[bootIDs], bootFdata$endrow[bootIDs]), 1, function(x)seq(x[1], x[2])))

  # select the rows decided above
  bootData <- bootFdata$Fdata[bootRows,]

  # we need to create a new ID for each bootstrap asset
  counter <- 0
  tmp <- apply(cbind(bootFdata$startrow[bootIDs], bootFdata$endrow[bootIDs]), 1, 
               function(x){ counter<<- counter+1; return(rep(counter, x[2]-x[1]+1))})
  ## Magic: the <<- assigns values to the global variable counter, which is external to the apply function.
  
  if(is.list(tmp)) bootData$ID <- unlist(tmp) ## if output of apply is a list, collapse it
  else bootData$ID <- as.vector(tmp) ## if output of apply is a vector (which it will be if every asset is inspected same number of times)
  # as.vector collapses a matrix into a vector by stacking the columns
  return(list(bootData=bootData, bootIDs=bootIDs, bootRows=bootRows))
  }



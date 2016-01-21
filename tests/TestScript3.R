#
# test bootstrap code.

library(transprob)
library(expm)
data(Embankment)
data = Embankment#[1:10,]
labels = c('A', 'B', 'C', 'D', 'E')

out <- bootstrap(data, labels, date_format="%d-%b-%Y", end_date=NULL, 
                      removeSingleton=FALSE, transInterval=1, ObservedOnly=FALSE, nboot=10)


bootstrap(data, labels, date_format="%d-%b-%Y", end_date=NULL, 
          removeSingleton=FALSE, transInterval=1, ObservedOnly=TRUE, nboot=10)


bootstrap(data, labels, date_format="%d-%b-%Y", end_date=NULL, 
          removeSingleton=TRUE, transInterval=1, ObservedOnly=FALSE, nboot=10)


labels2 = c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D')

out <- bootstrap(FinancialRating, labels2, removeSingleton = FALSE,  ObservedOnly = FALSE, nboot=10)

bootFdata <- transprob:::bootPrepData(FinancialRating, labels2, removeSingleton=FALSE, ObservedOnly=FALSE)

###########################################




Fdata = FormatData(data=data, labels=labels, date_format="%d-%b-%Y")

ExtractAsset(Fdata, 2)
IDtable <- as.data.frame(table(Fdata[,'ID']))
IDs = IDtable[,1]



#################### Very simple toy

IDs <- 1:6
ID=c(1,2,2,3,3,3,4,5,5,6,6,6,6)

rowindex <- 1:length(ID)
tmp <- data.frame(ID=ID)
end <-rowindex[c(as.logical(diff(tmp$ID, lag=1)), TRUE)] # add extra TRUE on end as diff is 1 element short and we want the last row
start <- rowindex[c(TRUE, as.logical(diff(tmp$ID, lag=1))) ] ## add extra TRUE on start


startT <-c (1,2,4,7,8,10) 
endT <- c(1,3,6,7,9,10, 13)

bootIDs <- sample(IDs, replace = TRUE)
bootRows <- unlist(apply(cbind(start=start[bootIDs], end=end[bootIDs]), 1, function(x)seq(x[1], x[2])))
bootData <- tmp[bootRows,]
# need to give new ID


#################### Using real data
data = Embankment[1:10,]
Fdata = FormatData(data=data, labels=labels, date_format="%d-%b-%Y")
IDs <- as.data.frame(table(Fdata[,'ID']))[,1]
rowindex <- 1:length(Fdata$ID)

## setup
diffs <- as.logical(diff(Fdata$ID, lag=1))

### the row in Fdata in which each asset entry ends
endrow <-rowindex[c(diffs, TRUE)] # add extra TRUE on end as diff is 1 element short and we want the last row

### the row in Fdata in which each asset entry begins
startrow <- rowindex[c(TRUE, diffs) ] ## add extra TRUE on start


###### Bootstrapping part
(bootIDs <- sample(IDs, replace = TRUE))
bootRows <- unlist(apply(cbind(startrow[bootIDs], endrow[bootIDs]), 1, function(x)seq(x[1], x[2])))
bootData <- Fdata[bootRows,]

# we need to create a new ID for each bootstrap asset
counter <- 0
bootData$ID <- unlist(apply(cbind(startrow[bootIDs], endrow[bootIDs]), 1, function(x){ counter<<- counter+1; return(rep(counter, x[2]-x[1]+1))}))

## check new function with old function
set.seed(1)
data = Embankment[1:10,]
Fdata = FormatData(data=data, labels=labels, date_format="%d-%b-%Y")
IDs <- as.data.frame(table(Fdata[,'ID']))[,1]
bootsampleSLOW(Fdata, IDs)


set.seed(1)
rowindex <- 1:length(Fdata$ID)
diffs <- as.logical(diff(Fdata$ID, lag=1))
endrow <-rowindex[c(diffs, TRUE)] # add extra TRUE on end as diff is 1 element short and we want the last row
startrow <- rowindex[c(TRUE, diffs) ] ## add extra TRUE on start
bootsample(Fdata, IDs, startrow, endrow )
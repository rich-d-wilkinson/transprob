

#
# NOTE: after clicking buil and reload, it doesn't recognize functions
# Have to also run devtools::document() - this fixes things
#
#
# To do
# - store earthworks data and check sensible answersgiven.
#
#



# Code agrees with matlab
#




# Are we dealing with assets that stay in same Rating for multiple observations? Yes




#' @export
FormatData <- function(data, labels, date_format="%d-%b-%Y", end_date=NULL){
  # data is a data frame consisting of at least three columns
  # Assume first column is ID
  # Second column is Date
  # Third column is Rating
  colnames(data)[1:3] <- c('ID', 'Date', 'Rating')
  #
  # Add something to deal with times as well?
  #
  
  
  if(!is.factor(data$Rating))  data$Rating = factor(data$Rating, levels=labels)
  data$Std_dates = strptime(data[,2],format = date_format)
  data$NumericRating = as.numeric(factor(data$Rating, levels=labels, labels=1:length(labels)))
  if(!is.null(end_date)){
    end_date = strptime(end_date,format = date_format)
    keep <- end_date>=data$Std_dates
    return(data[keep,])
    }
  
  
  # swap factor rating for numeric value (1-d), with order determined by labels
  # note fixed=TRUE needed else the expression match assumes regular expression
  return(data)
}


# Calculate transitions for each asset - i.e. a d*d matrix where d= number of classes


CalcTrans1ID <- function(data){
  return(table(data[-dim(data)[1],3],data[-1,3] ))
}


#' @export
ExtractAsset <- function(data, id){
  return(data[which(data[,'ID']==id),])
}


#' Depricated
#' 
#' CalcTrans is much quicker, but I've left this here for comparison
CalcTransOld <- function(data, IDs, labels){
  # Calculate the N_{ij} matrix of counts of the number of transitions between different classes
  idTotalsMat <- sapply(IDs[,1], function(x) CalcTrans1ID(ExtractAsset(data, x)))

  out <- matrix(rowSums(idTotalsMat), nr=length(labels))
  colnames(out) <- labels
  rownames(out) <- labels
  return(out)
}


CalcTransR<- function(data, labels){
  NN<- matrix(0,nrow=length(labels), ncol=length(labels))
  colnames(NN) <- rownames(NN) <- labels
  ndata <- dim(data)[1]
  for(ii in 1:(ndata-1)){
    if(data[ii,'ID']== data[ii+1, 'ID']){ ## i.e. if same asset
      NN[data[ii,'NumericRating'], data[ii+1,'NumericRating']] = NN[data[ii,'NumericRating'], data[ii+1,'NumericRating']]+1
    }
  }
return(NN)
}

#' @export
CalcTrans <- function(data, labels, Cpp=TRUE){
  if(Cpp) {
    NN <-CalcTransCpp(data$ID, data$Rating, 1:length(labels))
    colnames(NN) <- rownames(NN) <- labels
    return(NN)
  }
    else return( CalcTransR(data, labels))
}

###################################################
# Calculate transition times
###################################################
# Take first time for an asset as first inspection time
# take last time as last inspection time

secsperyear= 60 * 60 * 24 * 365.25


CalcTime1ID <- function(data, end_date=NULL, ObservedOnly=FALSE){
  # returns differences between successive dates in years
  # data must have 'Rating' as a column title

  if(ObservedOnly){
    data$diffs = c(diff(c(as.numeric(data$Std_dates)))/secsperyear, 0 ) #  put zero in last position
  } else{ data$diffs=diff(c(as.numeric(data$Std_dates), as.numeric(end_date)))/secsperyear }
  #return(summarize(group_by(data[,c('Rating', 'diffs')],Rating), sum(diffs)))

  # calculate the total amount of time in each rating.
  # much of this command is to ensure that 0's are given to factors that don't occur in the asset history
  tmp = plyr::ddply(data.frame(Rating=data$Rating, diffs=data$diffs), .(Rating), summarise, Times = sum(diffs), .drop=FALSE)


  rownames(tmp)=tmp[,1]
  return(dplyr::select(tmp, Times)) # return a matrix with row titles corresponding to the factors
  # this is so that the loop command works
    }

#
#
# Try just looping through instead
#
#

#' @export
CalcTime <- function(data, labels, end_date, ObservedOnly=FALSE, Cpp=TRUE){
  if(Cpp){
    if(ObservedOnly){
      TT=CalcTimeObservedOnlyCpp(data$ID, as.numeric(data$Std_dates), data$Rating, 1:length(labels), as.numeric(end_date))
    }else{
      TT = CalcTimeAllCpp(data$ID, as.numeric(data$Std_dates), data$Rating, 1:length(labels), as.numeric(end_date))
    }
    TT = matrix(TT, nc=1)
    rownames(TT) <- labels
  } else{ # use slower R implementation
    TT=CalcTimeR(data, labels, end_date, ObservedOnly)
  }
  return(TT)
}



CalcTimeR <- function(data, labels, end_date, ObservedOnly=FALSE){
  TT <- rep(0, length(labels))
  names(TT) <- labels
  ndata <- dim(data)[1]
  for(ii in 1:(ndata-1)){
    if(data[ii,'ID']== data[ii+1, 'ID']){ ## i.e. if two consecutive rows are same asset
        TT[data[ii,'Rating']] <- TT[data[ii,'Rating']] + (as.numeric(data$Std_dates[ii+1] -data$Std_dates[ii]))/365.25
      } else{ # last record of an asset
        if(!ObservedOnly) { # if ObservedOnly=FALSE calc time to end_date
          TT[data[ii,'Rating']] <- TT[data[ii,'Rating']] + (as.numeric(end_date -data$Std_dates[ii]))/365.25
        } # else do nothing
      }
  }
  if(!ObservedOnly){# deal with last row of matrix
    TT[data[ndata,'Rating']] <- TT[data[ndata,'Rating']] + (as.numeric(end_date -data$Std_dates[ndata]))/365.25
  }
  return(matrix(TT,ncol=1)) 
}


CalcTimeOld <- function(data, IDs, labels, end_date, ObservedOnly=FALSE){
  # Calculate the N_{ij} matrix of counts of the number of transitions between different classes
  idTotalsVec <- lapply(IDs[,1], function(x) CalcTime1ID(ExtractAsset(data, x), end_date, ObservedOnly))
  return(Reduce('+', idTotalsVec))
}


#' @export
CalcQ <- function(Trans, Times){
  Q =apply(Trans,2, function(x) x/Times[,1])
  Q[is.na(Q)]=0
  Q[is.infinite(Q)]=0 # in case any Times are zero
  diag(Q)=0 # incase we counted any identity transitions e.g.  A -> A
  diag(Q) = -rowSums(Q)
  return(Q)
  }


#' Calculate transition matrices of continuous time Markov chains.
#'
#' \code{transprob} calculates the transition matrix for a continuous time Markov chain. It is an implementation of the transprob MATLAB function.
#' It is more general than the MATLAB function as it allows for the removal of assets that are only observed once, and for only including
#' observed changes. I.e., if the last observation was 3 years ago, you have the choice whether to count those three years in the transition
#' calculation or not.
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
#'
#' @return The transition matrix, with the rows and columns in the order specified in labels
#' @examples
#' labels = c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D')
#' transprob(FinancialRating, labels, date_format="%d-%b-%Y", end_date = '10-Nov-2015', removeSingleton = TRUE)
#' transprob(FinancialRating, labels, date_format="%d-%b-%Y", removeSingleton = FALSE)
#' transprob(FinancialRating, labels, date_format="%d-%b-%Y", removeSingleton = FALSE, ObservedOnly=TRUE)
#'
#' @export
transprob <- function(data, labels, date_format="%d-%b-%Y", end_date=NULL, removeSingleton=TRUE, transInterval=1, ObservedOnly=FALSE, Cpp=TRUE, detail=FALSE){

  Fdata = FormatData(data=data, labels=labels, date_format, end_date)
  if(!is.null(end_date)) end_date = strptime(end_date, date_format) # put date in correct form
  else {
    end_date = max(Fdata$Std_dates)
   # print(paste('Assumed end date is ', end_date))
  }
  IDtable <- as.data.frame(table(Fdata[,'ID']))

  # Remove Singletons
  if(removeSingleton){

    nonSingletonID <- IDtable[IDtable[,2]>1,]
    Fdata <- Fdata[Fdata[,1] %in% nonSingletonID[,1],]
    singletonID <- IDtable[IDtable[,2]==1,]
    IDs = nonSingletonID
  } else IDs = IDtable

  
  Trans <- CalcTrans(Fdata,  labels, Cpp)
  Times = CalcTime(Fdata,  labels=labels, end_date, ObservedOnly, Cpp)
  Q = CalcQ(Trans,Times)
  if(detail) return(list(transMat=expm::expm(Q*transInterval), Trans=Trans, Times=Times ))
  return(expm::expm(Q*transInterval))
}


#' Transforms estimated transition matrices into upper diagonal matrices in a crude way
#'
#' Transforms estimate transition matrices to be upper diagonal either by adding all lower diagonal elements onto the diagonal 
#' of the same row, or by setting lower diagonal elements to 0 and rescaling the other row elements so that the row sum remains 1.
#' 
#' @param transmatrix an estimated transition matrix from transprob
#'
#' @param LoadDiag logical. If TRUE (default) lower diagonal elements are added onto the diagonal element in the same row. 
#' If FALSE, lower diagonal elements are set to zero and each row rescaled to ensure a row sum of 1.
#'
#' @export
DegradeOnly <-function(transmatrix, LoadDiag=TRUE){
  if(LoadDiag){
    tmp <- tmp2 <- transmatrix
    tmp[lower.tri(transmatrix)]<-0
    tmp2[upper.tri(transmatrix, diag = TRUE)]<-0
    return(tmp+diag(rowSums(tmp2)))
  }
  else{ ## set lower diagonal elements to zero and rescale each row so that it sums to 1.
    transmatrix[lower.tri(transmatrix)]<-0
    rowSums(transmatrix)
    return(sweep(transmatrix, 1, rowSums(transmatrix), FUN='/'))
  }
}

#' @useDynLib transprob
#' @importFrom Rcpp sourceCpp
NULL


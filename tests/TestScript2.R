library(transprob)
library(expm)
data(Embankment)
data = Embankment
labels = c('A', 'B', 'C', 'D', 'E')

system.time({T1=transprob(data, labels, date_format="%d-%b-%Y", removeSingleton = FALSE)})

system.time({T2=transprob(data, labels, date_format="%d-%b-%Y", removeSingleton = TRUE)})

system.time({T3=transprob(data, labels, date_format="%d-%b-%Y", removeSingleton = FALSE, ObservedOnly = TRUE)})
system.time({T4=transprob(data, labels, date_format="%d-%b-%Y", removeSingleton = TRUE, ObservedOnly = TRUE)})
# These two are the same

###########################################
Fdata = FormatData(data=data, labels=labels, date_format="%d-%b-%Y")



# Extract one asset
ExtractAsset(Fdata, 12)

# Calculate the transitions
transprob:::CalcTrans1ID(ExtractAsset(Fdata, 12))

# Remove Singletons
IDtable <- as.data.frame(table(Fdata[,'ID']))
nonSingletonID <- IDtable[IDtable[,2]>1,]
singletonID <- IDtable[IDtable[,2]==1,]

# Calculate transition counts including all IDs
#Trans <- transprob:::CalcTransOld(Fdata, IDtable, labels) # apply to all IDs
#Trans2 <- transprob:::CalcTransOld(Fdata, nonSingletonID, labels) # apply to non-singletons only
#Trans==Trans2 # should agree as singletons don't transition by definition.


#transprob:::CalcTransOld(Fdata, singletonID, labels) # should all be zero

system.time(CalcTrans(Fdata,  labels))
#system.time(print(transprob:::CalcTransOld(Fdata,  IDtable,labels)))

# Or equivalently
#CalcTrans(Fdata, labels, Cpp=TRUE)
#CalcTrans(Fdata, labels, Cpp=FALSE)


end_date = strptime('10-Nov-2015', "%d-%b-%Y")



#(Times = transprob:::CalcTimeOld(Fdata, IDtable , labels=labels, end_date, ObservedOnly=TRUE))
#CalcTime(Fdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=FALSE) # apply to all data
CalcTime(Fdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=TRUE)# apply to all data

#(Times = transprob:::CalcTimeOld(Fdata, IDtable , labels=labels, end_date, ObservedOnly=FALSE))
#CalcTime(Fdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=FALSE) # apply to all data
#CalcTime(Fdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=TRUE) # apply to all data


# No-singletons
nonSingletonFdata <- Fdata[Fdata[,1] %in% nonSingletonID[,1],]
#(Times = transprob:::CalcTimeOld(Fdata, nonSingletonID , labels=labels, end_date, ObservedOnly=FALSE))
#CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=FALSE)# apply to all data
CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=TRUE) # apply to all data

#(Times = transprob:::CalcTimeOld(Fdata, nonSingletonID , labels=labels, end_date, ObservedOnly=TRUE))
#CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=FALSE) # apply to all data
#CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=TRUE) # apply to all data


#system.time((Times = transprob:::CalcTimeOld(Fdata, nonSingletonID , labels=labels, end_date, ObservedOnly=FALSE)))
#system.time(CalcTime(nonSingletonFdata, labels, end_date, ObservedOnly=FALSE, Cpp=FALSE))
#system.time(CalcTime(nonSingletonFdata, labels, end_date, ObservedOnly=FALSE, Cpp=TRUE))



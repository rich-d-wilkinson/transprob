library(transprob)
library(expm)
data(FinancialRating)

labels = c('A', 'AA', 'AAA', 'B','BB','BBB','CCC','D')

transprob(FinancialRating, labels, date_format="%d-%b-%Y", end_date = '10-Nov-2015', removeSingleton = TRUE)
transprob(FinancialRating, labels, date_format="%d-%b-%Y", removeSingleton = TRUE, Cpp=FALSE)

system.time({TT1 = transprob(FinancialRating, labels, date_format="%d-%b-%Y", removeSingleton = TRUE, Cpp=FALSE)})
system.time({TT2 = transprob(FinancialRating, labels, date_format="%d-%b-%Y", removeSingleton = TRUE, Cpp=TRUE)})
max(TT1-TT2)

##
#
# 
# THESE SHOULD AGREE
#

transprob(FinancialRating, labels, date_format="%d-%b-%Y", end_date = '10-Nov-2015', removeSingleton = TRUE, ObservedOnly=TRUE)
transprob(FinancialRating, labels, date_format="%d-%b-%Y", removeSingleton = FALSE, ObservedOnly = TRUE)


#
#
# Now lets look in more detail at some of the commands
#
#

# Clean the data
Fdata = FormatData(data=FinancialRating, labels=labels, date_format="%d-%b-%Y")

# Extract one asset
ExtractAsset(Fdata, 14413)

# Calculate the transitions
transprob:::CalcTrans1ID(ExtractAsset(Fdata, 14413))


# Remove Singletons
IDtable <- as.data.frame(table(Fdata[,'ID']))
nonSingletonID <- IDtable[IDtable[,2]>1,]
singletonID <- IDtable[IDtable[,2]==1,]

# Calculate transition counts including all IDs
Trans <- transprob:::CalcTransOld(Fdata, IDtable, labels) # apply to all IDs
Trans2 <- transprob:::CalcTransOld(Fdata, nonSingletonID, labels) # apply to non-singletons only
Trans==Trans2 # should agree as singletons don't transition by definition.

transprob:::CalcTransOld(Fdata, singletonID, labels) # should all be zero
Trans==CalcTrans(Fdata,  labels) # Use faster new function
Trans== transprob:::CalcTransCpp(Fdata$ID, Fdata$NumericRating,  1:length(labels))

system.time(CalcTrans(Fdata,  labels))
system.time(transprob:::CalcTransOld(Fdata,  IDtable,labels))
system.time(transprob:::CalcTransCpp(Fdata$ID, Fdata$NumericRating,  1:length(labels)))

# Or equivalently
CalcTrans(Fdata, labels, Cpp=TRUE)
CalcTrans(Fdata, labels, Cpp=FALSE)

#####################################################################
#
# Transition times
#
#
#####################################################################



end_date = strptime('10-Nov-2015', "%d-%b-%Y")



#(Times = transprob:::CalcTimeOld(Fdata, IDtable , labels=labels, end_date, ObservedOnly=TRUE))
Times=CalcTime(Fdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=FALSE) # apply to all data
CalcTime(Fdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=TRUE) -Times# apply to all data

#(Times = transprob:::CalcTimeOld(Fdata, IDtable , labels=labels, end_date, ObservedOnly=FALSE))
Times=CalcTime(Fdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=FALSE)#-Times # apply to all data
CalcTime(Fdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=TRUE) -Times# apply to all data


# No-singletons
nonSingletonFdata <- Fdata[Fdata[,1] %in% nonSingletonID[,1],]
#(Times = transprob:::CalcTimeOld(Fdata, nonSingletonID , labels=labels, end_date, ObservedOnly=FALSE))
Times=CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=FALSE) # apply to all data
CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=FALSE, Cpp=TRUE) -Times# apply to all data

#(Times = transprob:::CalcTimeOld(Fdata, nonSingletonID , labels=labels, end_date, ObservedOnly=TRUE))
Times=CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=FALSE) # apply to all data
CalcTime(nonSingletonFdata, labels=labels, end_date, ObservedOnly=TRUE, Cpp=TRUE) -Times# apply to all data


#system.time((Times = transprob:::CalcTimeOld(Fdata, nonSingletonID , labels=labels, end_date, ObservedOnly=FALSE)))
system.time(CalcTime(nonSingletonFdata, labels, end_date, ObservedOnly=FALSE, Cpp=FALSE))
system.time(CalcTime(nonSingletonFdata, labels, end_date, ObservedOnly=FALSE, Cpp=TRUE))



########################
Times=CalcTime(nonSingletonFdata, labels, end_date, ObservedOnly=FALSE)

Q = CalcQ(Trans,Times)
expm(Q*1)


#
#
# MATLAB CODE
# load Data_TransProb
# data2 = data(1:10,:)
# transprob(data2, 'labels', {'A', 'AA', 'B','BB', 'BBB', 'CCC', 'D'}, 'endDate', '10-Nov-2015')
# ans =
  
#  89.5291    9.7147         0         0    0.7562         0         0
#        0   86.1549         0         0   13.8451         0         0
#        0         0   92.9100    3.6201         0    3.1285    0.3415
#        0         0    9.3907   90.4298         0    0.1677    0.0118
#        0         0         0         0  100.0000         0         0
#        0         0   15.9528    0.3296         0   67.0936   16.6240
#        0         0         0         0         0         0  100.0000
#
# SHOULD AGREE WITH OUTPUT FROM 
# transprob(FinancialRatings[1:10,], labels=c('A', 'AA',  'B','BB','BBB','CCC','D'), date_format="%d-%b-%Y", 
#            end_date = '10-Nov-2015', removeSingleton = TRUE)
#
# Their calculations are output by a fraction of a percent - I think either because of rounding errors in expm, or because MATLAB is accounting for leapyears when we are not
#

# MATLAB CODE
# load Data_TransProb
# transprob(data, 'labels', {'A', 'AA', 'AAA','B','BB', 'BBB', 'CCC', 'D'}, 'endDate', '10-Nov-2015')
#ans =

# 94.9729    1.8788    0.0745    0.0389    0.3270    2.6603    0.0012    0.0465
# 3.0025   95.3244    1.1067    0.0025    0.1058    0.4318    0.0002    0.0262
# 0.6028    4.5172   94.7244    0.0006    0.0264    0.1276    0.0001    0.0009
# 0.0710    0.0032    0.0005   90.0166    5.5817    0.6133    1.9105    1.8032
# 0.4266    0.0799    0.0155    2.5773   91.8504    4.3380    0.2071    0.5051
# 3.1602    0.1268    0.0137    0.2868    2.3967   93.8451    0.0384    0.1323
# 0.0068    0.0006    0.0001    3.8265    1.2296    0.2155   83.5910   11.1298
#      0         0         0         0         0         0         0  100.0000
#
#
# SHOULD AGREE WITH OUTPUT FROM 
# transprob(FinancialRating, labels, date_format="%d-%b-%Y", end_date = '10-Nov-2015', removeSingleton = FALSE)*100

data <- read.csv('Embankments_data_RWJP.txt')

# remove year column
data <- data[,c(1,2,4) ]


dates <- as.character(data[,2])

split = strsplit(dates, " ")
split <- unlist(split)
dates <- split[seq(1, length(split),2)]
Newdates <- as.Date(dates, format="%d/%m/%Y")
dates <-format(Newdates, "%d-%b-%Y")

EHCB<- data[,3]
EHCB <- gsub("1", "A", EHCB)
EHCB <- gsub("2", "B", EHCB)
EHCB <- gsub("3", "C", EHCB)
EHCB <- gsub("4", "D", EHCB)
EHCB <- gsub("5", "E", EHCB)


Embankment <- data.frame('ID'= data[,1], 'Date'=dates, 'EHC'=EHCB)


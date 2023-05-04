


## 01 Read raw data ####

## Get the files and paths
folders <- list.files('data/')
files <- list()
paths <- character()

for(i in 1:length(folders)){
  files[[i]] <- list.files(paste('data/', folders[i], '/', sep=''))
}

for(i in 1:length(folders)) {
  for( j in 1: length(files[[i]])) {
    paths <- c(paths,  paste('data/', folders[i], '/', files[[i]][j], sep=''))
  }
}

## Make a holder for the raw data
names <- c('caseid','date','event','comment','country1',
           'country2','species', 'order')
masterdata <- data.frame(matrix(nrow=0, ncol = length(names)))
colnames(masterdata) <- names

## 02 Big old for() loop to rbind() data
linesperfile <- numeric()

for( i in 1:length(paths)){
  # read in data for each iteration
  datatemp <- readLines(paths[i], warn = F)
  
  # count the rows based on date
  myrows <- grep(pattern = "[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]",
                 x=datatemp)
  linesperfile <- c(linesperfile, length(myrows))
  # which rows have 6 or 7 fields (remember 1st line has 2 fields)
  rowslength <- c(myrows[-1] - myrows[-length(myrows)],  
                  length(datatemp) - myrows[length(myrows)]+1)
  
  # initialize
  
  mydata <- data.frame(matrix(nrow=length(myrows), ncol = 7))
  names <- c('caseid','date','event','comment','country1','country2','species')
  colnames(mydata) <- names
  
  for(i in 1:length(myrows)){
    
    if(rowslength[i] == 5) {
      mydata$caseid[i] <- strsplit(x = datatemp[myrows[i]+0], '\t')[[1]][1]     # caseid
      mydata$date[i] <- strsplit(x = datatemp[myrows[i]+0], '\t')[[1]][2]       # date
      mydata$event[i] <- gsub('\\','', x = datatemp[myrows[i]+1], fixed = T)    # event
      mydata$comment[i] <- gsub('\\','', x = datatemp[myrows[i]+2], fixed = T)  # comment
      mydata$country1[i] <- gsub('\\','', x = datatemp[myrows[i]+3], fixed = T) # country1
      mydata$country2[i] <- NA                                             # country2
      mydata$species[i] <- gsub('\\','', x = datatemp[myrows[i]+4], fixed = T)  # species
      mydata$order[i] <- NA
    }
    
    if(rowslength[i] == 6) {
      mydata$caseid[i] <- strsplit(x = datatemp[myrows[i]+0], '\t')[[1]][1]     # caseid
      mydata$date[i] <- strsplit(x = datatemp[myrows[i]+0], '\t')[[1]][2]       # date
      mydata$event[i] <- gsub('\\','', x = datatemp[myrows[i]+1], fixed = T)    # event
      mydata$comment[i] <- gsub('\\','', x = datatemp[myrows[i]+2], fixed = T)  # comment
      mydata$country1[i] <- gsub('\\','', x = datatemp[myrows[i]+3], fixed = T) # country1
      mydata$country2[i] <- gsub('\\','', x = datatemp[myrows[i]+4], fixed = T) # country2
      mydata$species[i] <- gsub('\\','', x = datatemp[myrows[i]+5], fixed = T)  # species
      mydata$order[i] <- NA
    }
  }
  
  masterdata <- rbind(masterdata, mydata)
  
}

# linesperfile - numeric vector of how many rows per file
# files - list of files in each folder
# folders - folder names by order

masterdata$order <- c(
  rep('Artiodactyla', sum(linesperfile[1:2])),
  rep('Carnivora', sum(linesperfile[3:7])),
  rep('Perissodactyla', sum(linesperfile[8:9])),
  rep('Pholidota', sum(linesperfile[10:11])),
  rep('Proboscidea', sum(linesperfile[12:16]))
  )

masterdata$date2 <- dmy(masterdata$date)
masterdata$year <- year(masterdata$date2)
masterdata$post2017 <- masterdata$date2 >= dmy('31/12/2016')

masterdata$decade <- character(nrow(masterdata))
masterdata$decade[which(masterdata$year < 1994)] <- '1993'
masterdata$decade[which(masterdata$year >= 1994 & masterdata$year < 2004)] <- '2003'
masterdata$decade[which(masterdata$year >= 2004 & masterdata$year < 2014)] <- '2013'
masterdata$decade[which(masterdata$year >= 2014)] <- '2023'
table(masterdata$decade)
table(masterdata$year)

masterdata$continent <- character(nrow(masterdata))

temp <- which(masterdata$country1 == 'South Africa' |
        masterdata$country1 == 'Tanzania, United Replublic of' |
        masterdata$country1 == 'Kenya' | 
        masterdata$country1 == 'Zimbabwe' |
        masterdata$country1 == 'Namibia')
masterdata$continent[temp] <- 'Africa'
masterdata$continent[-temp] <- 'Asia'

table(masterdata$continent)

rm(files, mydata, datatemp, folders, i, j, myrows, names, 
   paths, rowslength, linesperfile)

## xx Trash code ####
# sum(linesperfile)
# 
# length(res)
# res[1]
# 


# str_match(pattern = "[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]",
#      string=res[1])
# 
# grepl(pattern = "[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]",
#           x=res[1])
# grep(pattern = "[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]",
#       x=res[1])


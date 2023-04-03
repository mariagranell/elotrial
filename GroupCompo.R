# Group composition 
# Select packages
library(dplyr) # to remove duplicates
library(writexl) # to save as excel file
library(reshape2)
library(data.table)

# Import data
LHdata <- read.csv2("/Users/mariagranell/Repositories/data_summaries/life_history/IVP_Lifehistory_020822.csv", header = T, stringsAsFactors = F, na.strings = c('NA', 'Not yet'))

Date <- seq(as.Date("2020-01-01"), Sys.Date(), by="days")
dfDate <- as.data.frame(Date)

# Look at data
summary(LHdata)
str(LHdata)

# Select data
LHdata <- LHdata[,c(3,5,7,8,10:26)]
dfDate$Date <- as.Date(format(as.POSIXct(dfDate$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
str(LHdata)
str(dfDate)

# Create "LastSeen" column to add current date if ind still present in a group
LHdata$LastSeen <- NA

for(i in 1:nrow(LHdata)) {
  ifelse(nchar(LHdata$PresentGp[i]) == 2, 
         LHdata$LastSeen[i] <- format(Sys.Date(), "%d.%m.%Y"), 
         LHdata$LastSeen[i] <- NA)
}

# Check that it worked
which(nchar(LHdata$PresentGp) == 2)
head(LHdata[c(which(nchar(LHdata$PresentGp) == 2)),]) # OK
head(LHdata)

# Format dates using the following output = YYYY-mm-dd
LHdata$DOB <- as.Date(format(as.POSIXct(LHdata$DOB, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$FirstRecorded <- as.Date(format(as.POSIXct(LHdata$FirstRecorded, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DepartureNatalGp <- as.Date(format(as.POSIXct(LHdata$DepartureNatalGp,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration1 <- as.Date(format(as.POSIXct(LHdata$DateImmigration1,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen1 <- as.Date(format(as.POSIXct(LHdata$LastSeen1, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration2 <- as.Date(format(as.POSIXct(LHdata$DateImmigration2,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen2 <- as.Date(format(as.POSIXct(LHdata$LastSeen2, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration3 <- as.Date(format(as.POSIXct(LHdata$DateImmigration3,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen3 <- as.Date(format(as.POSIXct(LHdata$LastSeen3, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$DateImmigration4 <- as.Date(format(as.POSIXct(LHdata$DateImmigration4,format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen4 <- as.Date(format(as.POSIXct(LHdata$LastSeen4, format = "%d/%m/%Y"), "%Y-%m-%d"))
LHdata$LastSeen <- as.Date(format(as.POSIXct(LHdata$LastSeen, format = "%d/%m/%Y"), "%Y-%m-%d"))
#LHdata$ImmigrationGp1 <- as.factor(LHdata$ImmigrationGp1)
#LHdata$ImmigrationGp2 <- as.factor(LHdata$ImmigrationGp2)
#LHdata$ImmigrationGp3 <- as.factor(LHdata$ImmigrationGp3)
#LHdata$ImmigrationGp4 <- as.factor(LHdata$ImmigrationGp4)
str(LHdata)
#View(LHdata)

# Create a column with the year the individual was either born (DOB) or first seen

# DOB has two types of input: Year and Day/Month/Year. We want to use both these types of input in a new column
# We therefore first make two columns: DOB2 with the input that is just Year and DOB3 with the input that is D/M/Y
LHdata$YearDOB2 <- as.numeric(format(as.POSIXct(LHdata$DOB, format = "%Y"), "%Y"))
LHdata$YearDOB3 <- as.numeric(format(as.POSIXct(LHdata$DOB, format = "%d/%m/%Y"), "%Y"))

# DOB2 also has input of the D/M/Y format. Delete these
LHdata$YearDOB2[LHdata$YearDOB2 < 1000] <- NA

# Only select the lowest value of DOB2 and DOB3 into new column DOB
LHdata$YearDOB <- apply(X = LHdata[,23:24],                     
                        MARGIN = 1,                           
                        FUN = function(x) min(x,na.rm = T) 
)
# This gives you a warning for every row where R encounters an NA
# You can therefore ignore this warning
str(LHdata)

# Apply function gives Inf as output for rows that had NA. Get rid of these by selecting only values below 3000 (since it's 2022 ;) 
LHdata$YearDOB[LHdata$YearDOB > 3000] <- NA

# Create a column with the year the individual was first recorded
LHdata$YearFR <- as.numeric(format(as.POSIXct(LHdata$FirstRecorded, format = "%d/%m/%Y"), "%Y"))

# New males that immigrate are already adults when they disperse. Their year of birth is therefore at least 4 years before first recorded
#LHdata$DateImmigration1 <- as.numeric(LHdata$DateImmigration1)
i = 1
LHdata$YearBO <- NA

for(i in 1:nrow(LHdata)) {
  if(is.na(LHdata$YearDOB[i])){
    if(is.na(LHdata$DateImmigration1[i])){
      LHdata$YearBO[i] <- NA
    } else {
      LHdata$YearBO[i] <- LHdata$YearFR[i] - 6
    }
  } else {
    LHdata$YearBO[i] <- LHdata$YearDOB[i]
  }
}


# Create a column with the year the individual was first seen or when it was born (first recording of the individual)
LHdata$Year <- apply(X = LHdata[,25:27],                     
                     MARGIN = 1,                           
                     FUN = function(x) min(x,na.rm = T) 
)
# Again, everytime R encounters an NA it returns Inf. You can ignore this
str(LHdata)

# Get rid of NA's
LHdata <- LHdata[!(is.na(LHdata$Code)),]

# Add age per year
LHdata[,c(29:42)] <- NA
LHdata <- LHdata[,c(1:4,5,6,9:22,28:42)]
colnames(LHdata)[22:35] <- c("TotalDurationAK","A2010","A2011","A2012","A2013","A2014","A2015","A2016","A2017","A2018","A2019","A2020","A2021", "A2022")

LHdata$A2010 <- 2009 - LHdata$Year
LHdata$A2011 <- 2010 - LHdata$Year
LHdata$A2012 <- 2011 - LHdata$Year
LHdata$A2013 <- 2012 - LHdata$Year
LHdata$A2014 <- 2013 - LHdata$Year
LHdata$A2015 <- 2014 - LHdata$Year
LHdata$A2016 <- 2015 - LHdata$Year
LHdata$A2017 <- 2016 - LHdata$Year
LHdata$A2018 <- 2017 - LHdata$Year
LHdata$A2019 <- 2018 - LHdata$Year
LHdata$A2020 <- 2019 - LHdata$Year
LHdata$A2021 <- 2020 - LHdata$Year
LHdata$A2022 <- 2021 - LHdata$Year

#LHdata[22:35] <- replace(LHdata[22:35], LHdata[22:35] < 0 , NA)

# Create a vector with our groups
Gp <- c("AK","BD","CR","KB","LT","NH","RC","RL", "IF")
Gp <- as.factor(Gp)
#levels(Gp)
#nlevels(Gp)

# create an empty list where results will be stored for each group
LH <- list()

# create loops that will separate the data by groups 
# (using all the different columns as one ind can be in a group at different time of their life: 
# PresentGp,BirthGp,ImmigrationGp1, 2, 3 & 4)

for(i in 1:nlevels(Gp)) {
  
  for(i in 1:nrow(LHdata)) {
    
    PG <- which(LHdata$PresentGp == Gp[i])
    BG <- which(LHdata$BirthGp == Gp[i])
    IG1 <- which(LHdata$ImmigrationGp1 == Gp[i])
    IG2 <- which(LHdata$ImmigrationGp2 == Gp[i])
    IG3 <- which(LHdata$ImmigrationGp3 == Gp[i])
    IG4 <- which(LHdata$ImmigrationGp4 == Gp[i])
    
    output <- rbind(LHdata[PG,],LHdata[BG,],LHdata[IG1,],LHdata[IG2,],LHdata[IG3,],LHdata[IG4,])
    
    LH[[i]] <- output 
    
  }
}



# Extract data for the different groups, removing duplicates & sorting it out by alphabetical order 
# for individuals and resetting row numbers

# create empty list + data frames where results will be stored for each group
LHGp <- data.frame()
listLHGp <- list()

for(i in 1:nlevels(Gp)) {
  
  LHGp <- distinct(as.data.frame(LH[i]))
  LHGp <- LHGp[order(LHGp$Code),]
  rownames(LHGp) <- NULL
  listLHGp[[i]] <- LHGp
  
}


# extract data for each gp
LHAK <- as.data.frame(listLHGp[1]) # 117 ind total present at least once in AK
LHBD <- as.data.frame(listLHGp[2]) # 206 ind total present at least once in BD
LHCR <- as.data.frame(listLHGp[3]) # 132 ind total present at least once in CR
LHKB <- as.data.frame(listLHGp[4]) # 72 ind total present at least once in KB
LHLT <- as.data.frame(listLHGp[5]) # 135 ind total present at least once in LT
LHNH <- as.data.frame(listLHGp[6]) # 161 ind total present at least once in NH
LHRC <- as.data.frame(listLHGp[7]) # 33 ind total present at least once in RC
LHRL <- as.data.frame(listLHGp[8]) # 2 ind total present at least once in RL
LHIF <- as.data.frame(listLHGp[9]) # 6 ind total present at aleast once in IF


#### ANKHASE ####

# add StartAK & EndAK + check structure
LHAK$StartAK <- NA
LHAK$StartAK <- as.Date(LHAK$StartAK, format = "%d.%m.%Y")
LHAK$EndAK <- NA
LHAK$EndAK <- as.Date(LHAK$EndAK, format = "%d.%m.%Y")
str(LHAK)


# extract start date for all the AK individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)
i = 1

for(i in 1:nrow(LHAK)) {
  
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK") {
    LHAK$StartAK[i] <- LHAK$DOB[i]} 
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK" & is.na(LHAK$DOB[i])) {
    LHAK$StartAK[i] <- LHAK$FirstRecorded[i]} 
  if (LHAK$ImmigrationGp2[i] == "AK" & !is.na(LHAK$DateImmigration2[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration2[i]} 
  if (LHAK$ImmigrationGp3[i] == "AK" & !is.na(LHAK$DateImmigration3[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration3[i]} 
  if (LHAK$ImmigrationGp4[i] == "AK" & !is.na(LHAK$DateImmigration4[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration4[i]} 
  if (LHAK$ImmigrationGp1[i] == "AK" & !is.na(LHAK$DateImmigration1[i])) {
    LHAK$StartAK[i] <- LHAK$DateImmigration1[i]} 
}



# extract end date for all the AK ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHAK)) {
  
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$DepartureNatalGp[i]} 
  if (!is.na(LHAK$ImmigrationGp1[i]) & (LHAK$ImmigrationGp1[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen1[i]}
  if (!is.na(LHAK$BirthGp[i]) & (LHAK$BirthGp[i]) == "AK" & is.na(LHAK$DepartureNatalGp[i])) {
    LHAK$EndAK[i] <- LHAK$LastSeen1[i]} 
  if (!is.na(LHAK$ImmigrationGp2[i]) & (LHAK$ImmigrationGp2[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen2[i]} 
  if (!is.na(LHAK$ImmigrationGp3[i]) & (LHAK$ImmigrationGp3[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen3[i]} 
  if (!is.na(LHAK$ImmigrationGp4[i]) & (LHAK$ImmigrationGp4[i]) == "AK") {
    LHAK$EndAK[i] <- LHAK$LastSeen4[i]}
  if (!is.na(LHAK$PresentGp[i]) & LHAK$PresentGp[i] == "AK" & is.na(LHAK$EndAK[i]))  {LHAK$EndAK[i] <- LHAK$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
AK <- LHAK[,c(1,2,33:37)]
#AK[,c(6:19)] <- NA
#colnames(AK) <- c("Individidual","Sex","DOB","StartAK", "EndAK","TotalDurationAK","A2010","A2011","A2012","A2013","A2014","A2015","A2016","A2017","A2018","A2019","A2020","A2021", "A2022")
head(AK)

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
AK$EndAK[is.na(AK$EndAK)] <- Sys.Date()

# Create a table with date on the y-axes - #AM #AF #SM #JM #JF #BB on x-axes

adjuv <- function(Sex, Age) {
  i = 1
  Adjuv <- list(nrow(AK))
  for(i in 1:nrow(AK))
    if (is.na(Sex[i]) | is.na(Age[i])) {
      Adjuv[i] <- NA
    } else {
      if(Sex[i] == "F" && Age[i] >= 4){
        Adjuv[i] <- "AF"
      } else {
        if(Sex[i] == "F" && Age[i] < 4 && Age[i] >= 0){
          Adjuv[i] <- "JF"
        } else {
          if(Sex[i] == "M" && Age[i] <= 3 && Age[i] >= 0){
            Adjuv[i] <- "JM"
          } else {
            if(Sex[i] == "M" && Age[i] == 4){
              Adjuv[i] <- "SM"
            } else {
              if(Sex[i] == "M" && Age[i] > 4){
                Adjuv[i] <- "AM"
              } else {
                if(Age[i] == -1){
                  Adjuv[i] <- "BB"
                } else { 
                Adjuv[i] <- NA
                }
              }
            }
          }
        }
      }
      
    }
  return(Adjuv)
}                          


AK$AJ2020 <- as.factor(do.call(rbind, adjuv(AK$Sex, AK$A2020)))
AK$AJ2021 <- as.factor(do.call(rbind, adjuv(AK$Sex, AK$A2021)))
AK$AJ2022 <- as.factor(do.call(rbind, adjuv(AK$Sex, AK$A2022)))


# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = AK$Code,
  Start = AK$StartAK,
  End = AK$EndAK
)


# Create the matrix
AKpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

#str(AKpresence)
# Function to replace 1 with adjuv

get_adjuv <- function(date, code){
  if (date == 2020){
  adjuv <- AK[AK[,1] == code,]$AJ2020}
  if (date == 2021){
    adjuv <- AK[AK[,1] == code,]$AJ2021}
  if (date == 2022){
    adjuv <- AK[AK[,1] == code,]$AJ2022}
  
  return(adjuv)
}

codes <- colnames(AKpresence)[1:ncol(AKpresence)]

for (i in (1:nrow(AKpresence))){
  date <- as.numeric(format(as.POSIXct(AKpresence$Date[i], format = "%Y-%m-%d"), "%Y"))
  for (j in (2:ncol(AKpresence))) {
    code = codes[j]
    AKpresence[[j]] <- as.character(AKpresence[[j]])
    if(!is.na(AKpresence[[j]][i]) && AKpresence[[j]][i] == "1"){
      adjuv <- get_adjuv(date, code)
      AKpresence[i, c(code)] <- as.character(adjuv)
    }
  }
}


# Count all adjuvs

AKpresence$AM <- apply(AKpresence, 1, function(x) length(which(x=="AM")))
AKpresence$AF <- apply(AKpresence, 1, function(x) length(which(x=="AF")))
AKpresence$ASR <- AKpresence$AM/(AKpresence$AF+AKpresence$AM)
AKpresence$SM <- apply(AKpresence, 1, function(x) length(which(x=="SM")))
AKpresence$JF <- apply(AKpresence, 1, function(x) length(which(x=="JF")))
AKpresence$JM <- apply(AKpresence, 1, function(x) length(which(x=="JM")))
AKpresence$BB <- apply(AKpresence, 1, function(x) length(which(x=="BB")))

AKCompo <- AKpresence[,c("Date", "ASR", "AM", "AF", "SM", "JF", "JM", "BB")]

#### BAIE DANKIE ####

# add StartBD & EndBD + check structure
LHBD$StartBD <- NA
LHBD$StartBD <- as.Date(LHBD$StartBD, format = "%d.%m.%Y")
LHBD$EndBD <- NA
LHBD$EndBD <- as.Date(LHBD$EndBD, format = "%d.%m.%Y")
str(LHBD)

# extract start date for all the BD individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHBD)) {
  
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD") {
    LHBD$StartBD[i] <- LHBD$DOB[i]} 
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD" & is.na(LHBD$DOB[i])) {
    LHBD$StartBD[i] <- LHBD$FirstRecorded[i]} 
  if (LHBD$ImmigrationGp1[i] == "BD" & !is.na(LHBD$DateImmigration1[i])) {
    LHBD$StartBD[i] <- LHBD$DateImmigration1[i]} 
  if (LHBD$ImmigrationGp2[i] == "BD" & !is.na(LHBD$DateImmigration2[i])) {
    LHBD$StartBD[i] <- LHBD$DateImmigration2[i]} 
  if (LHBD$ImmigrationGp3[i] == "BD" & !is.na(LHBD$DateImmigration3[i])) {
    LHBD$StartBD[i] <- LHBD$DateImmigration3[i]} 
  if (LHBD$ImmigrationGp4[i] == "BD" & !is.na(LHBD$DateImmigration4[i])) {
    LHBD$StartBD[i] <- LHBD$DateImmigration4[i]} 
  
}

# extract end date for all the BD ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHBD)) {
  
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$DepartureNatalGp[i]} 
  if (!is.na(LHBD$ImmigrationGp1[i]) & (LHBD$ImmigrationGp1[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen1[i]}
  if (!is.na(LHBD$BirthGp[i]) & (LHBD$BirthGp[i]) == "BD" & is.na(LHBD$DepartureNatalGp[i])) {
    LHBD$EndBD[i] <- LHBD$LastSeen1[i]} 
  if (!is.na(LHBD$ImmigrationGp2[i]) & (LHBD$ImmigrationGp2[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen2[i]} 
  if (!is.na(LHBD$ImmigrationGp3[i]) & (LHBD$ImmigrationGp3[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen3[i]} 
  if (!is.na(LHBD$ImmigrationGp4[i]) & (LHBD$ImmigrationGp4[i]) == "BD") {
    LHBD$EndBD[i] <- LHBD$LastSeen4[i]}
  if (!is.na(LHBD$PresentGp[i]) & LHBD$PresentGp[i] == "BD" & is.na(LHBD$EndBD[i]))  {LHBD$EndBD[i] <- LHBD$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
BD <- LHBD[,c(1,2,33:37)]

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
BD$EndBD[is.na(BD$EndBD)] <- Sys.Date()

# Create a table with date on the y-axes - #AM #AF #SM #JM #JF #BB on x-axes

adjuv <- function(Sex, Age) {
  i = 1
  Adjuv <- list(nrow(BD))
  for(i in 1:nrow(BD))
    if (is.na(Sex[i]) | is.na(Age[i])) {
      Adjuv[i] <- NA
    } else {
      if(Sex[i] == "F" && Age[i] >= 4){
        Adjuv[i] <- "AF"
      } else {
        if(Sex[i] == "F" && Age[i] < 4 && Age[i] >= 0){
          Adjuv[i] <- "JF"
        } else {
          if(Sex[i] == "M" && Age[i] <= 3 && Age[i] >= 0){
            Adjuv[i] <- "JM"
          } else {
            if(Sex[i] == "M" && Age[i] == 4){
              Adjuv[i] <- "SM"
            } else {
              if(Sex[i] == "M" && Age[i] > 4){
                Adjuv[i] <- "AM"
              } else {
                if(Age[i] == -1){
                  Adjuv[i] <- "BB"
                } else { 
                  Adjuv[i] <- NA
                }
              }
            }
          }
        }
      }
      
    }
  return(Adjuv)
}                          


BD$AJ2020 <- as.factor(do.call(rbind, adjuv(BD$Sex, BD$A2020)))
BD$AJ2021 <- as.factor(do.call(rbind, adjuv(BD$Sex, BD$A2021)))
BD$AJ2022 <- as.factor(do.call(rbind, adjuv(BD$Sex, BD$A2022)))


# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = BD$Code,
  Start = BD$StartBD,
  End = BD$EndBD
)


# Create the matrix
BDpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

#str(BDpresence)
# Function to replace 1 with adjuv

get_adjuv <- function(date, code){
  if (date == 2020){
    adjuv <- BD[BD[,1] == code,]$AJ2020}
  if (date == 2021){
    adjuv <- BD[BD[,1] == code,]$AJ2021}
  if (date == 2022){
    adjuv <- BD[BD[,1] == code,]$AJ2022}
  
  return(adjuv)
}

codes <- colnames(BDpresence)[1:ncol(BDpresence)]

for (i in (1:nrow(BDpresence))){
  date <- as.numeric(format(as.POSIXct(BDpresence$Date[i], format = "%Y-%m-%d"), "%Y"))
  for (j in (2:ncol(BDpresence))) {
    code = codes[j]
    BDpresence[[j]] <- as.character(BDpresence[[j]])
    if(!is.na(BDpresence[[j]][i]) && BDpresence[[j]][i] == "1"){
      adjuv <- get_adjuv(date, code)
      BDpresence[i, c(code)] <- as.character(adjuv)
    }
  }
}


# Count all adjuvs

BDpresence$AM <- apply(BDpresence, 1, function(x) length(which(x=="AM")))
BDpresence$AF <- apply(BDpresence, 1, function(x) length(which(x=="AF")))
BDpresence$ASR <- BDpresence$AM/(BDpresence$AF+BDpresence$AM)
BDpresence$SM <- apply(BDpresence, 1, function(x) length(which(x=="SM")))
BDpresence$JF <- apply(BDpresence, 1, function(x) length(which(x=="JF")))
BDpresence$JM <- apply(BDpresence, 1, function(x) length(which(x=="JM")))
BDpresence$BB <- apply(BDpresence, 1, function(x) length(which(x=="BB")))

BDCompo <- BDpresence[,c("Date", "ASR", "AM", "AF", "SM", "JF", "JM", "BB")]

#### CROSSING ####

# add StartCR & EndCR + check structure
LHCR$StartCR <- NA
LHCR$StartCR <- as.Date(LHCR$StartCR, format = "%d.%m.%Y")
LHCR$EndCR <- NA
LHCR$EndCR <- as.Date(LHCR$EndCR, format = "%d.%m.%Y")
str(LHCR)

# extract start date for all the CR individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHCR)) {
  
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR") {
    LHCR$StartCR[i] <- LHCR$DOB[i]} 
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR" & is.na(LHCR$DOB[i])) {
    LHCR$StartCR[i] <- LHCR$FirstRecorded[i]} 
  if (LHCR$ImmigrationGp1[i] == "CR" & !is.na(LHCR$DateImmigration1[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration1[i]} 
  if (LHCR$ImmigrationGp2[i] == "CR" & !is.na(LHCR$DateImmigration2[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration2[i]} 
  if (LHCR$ImmigrationGp3[i] == "CR" & !is.na(LHCR$DateImmigration3[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration3[i]} 
  if (LHCR$ImmigrationGp4[i] == "CR" & !is.na(LHCR$DateImmigration4[i])) {
    LHCR$StartCR[i] <- LHCR$DateImmigration4[i]} 
  
}

# extract end date for all the CR ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHCR)) {
  
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$DepartureNatalGp[i]} 
  if (!is.na(LHCR$ImmigrationGp1[i]) & (LHCR$ImmigrationGp1[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen1[i]}
  if (!is.na(LHCR$BirthGp[i]) & (LHCR$BirthGp[i]) == "CR" & is.na(LHCR$DepartureNatalGp[i])) {
    LHCR$EndCR[i] <- LHCR$LastSeen1[i]} 
  if (!is.na(LHCR$ImmigrationGp2[i]) & (LHCR$ImmigrationGp2[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen2[i]} 
  if (!is.na(LHCR$ImmigrationGp3[i]) & (LHCR$ImmigrationGp3[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen3[i]} 
  if (!is.na(LHCR$ImmigrationGp4[i]) & (LHCR$ImmigrationGp4[i]) == "CR") {
    LHCR$EndCR[i] <- LHCR$LastSeen4[i]}
  if (!is.na(LHCR$PresentGp[i]) & LHCR$PresentGp[i] == "CR" & is.na(LHCR$EndCR[i]))  {LHCR$EndCR[i] <- LHCR$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
CR <- LHCR[,c(1,2,33:37)]
#CR[,c(6:19)] <- NA
#colnames(CR) <- c("Individidual","Sex","DOB","StartCR", "EndCR","TotalDurationCR","A2010","A2011","A2012","A2013","A2014","A2015","A2016","A2017","A2018","A2019","A2020","A2021", "A2022")
head(CR)

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
CR$EndCR[is.na(CR$EndCR)] <- Sys.Date()

# Create a table with date on the y-axes - #AM #AF #SM #JM #JF #BB on x-axes

adjuv <- function(Sex, Age) {
  i = 1
  Adjuv <- list(nrow(CR))
  for(i in 1:nrow(CR))
    if (is.na(Sex[i]) | is.na(Age[i])) {
      Adjuv[i] <- NA
    } else {
      if(Sex[i] == "F" && Age[i] >= 4){
        Adjuv[i] <- "AF"
      } else {
        if(Sex[i] == "F" && Age[i] < 4 && Age[i] >= 0){
          Adjuv[i] <- "JF"
        } else {
          if(Sex[i] == "M" && Age[i] <= 3 && Age[i] >= 0){
            Adjuv[i] <- "JM"
          } else {
            if(Sex[i] == "M" && Age[i] == 4){
              Adjuv[i] <- "SM"
            } else {
              if(Sex[i] == "M" && Age[i] > 4){
                Adjuv[i] <- "AM"
              } else {
                if(Age[i] == -1){
                  Adjuv[i] <- "BB"
                } else { 
                  Adjuv[i] <- NA
                }
              }
            }
          }
        }
      }
      
    }
  return(Adjuv)
}                          


CR$AJ2020 <- as.factor(do.call(rbind, adjuv(CR$Sex, CR$A2020)))
CR$AJ2021 <- as.factor(do.call(rbind, adjuv(CR$Sex, CR$A2021)))
CR$AJ2022 <- as.factor(do.call(rbind, adjuv(CR$Sex, CR$A2022)))


# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = CR$Code,
  Start = CR$StartCR,
  End = CR$EndCR
)


# Create the matrix
CRpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

CRpresence <- CRpresence[,!("V1")]

#str(CRpresence)
# Function to replace 1 with adjuv

get_adjuv <- function(date, code){
  if (date == 2020){
    adjuv <- CR[CR[,1] == code,]$AJ2020}
  if (date == 2021){
    adjuv <- CR[CR[,1] == code,]$AJ2021}
  if (date == 2022){
    adjuv <- CR[CR[,1] == code,]$AJ2022}
  
  return(adjuv)
}

codes <- colnames(CRpresence)[1:ncol(CRpresence)]

for (i in (1:nrow(CRpresence))){
  date <- as.numeric(format(as.POSIXct(CRpresence$Date[i], format = "%Y-%m-%d"), "%Y"))
  for (j in (2:ncol(CRpresence))) {
    code = codes[j]
    CRpresence[[j]] <- as.character(CRpresence[[j]])
    if(!is.na(CRpresence[[j]][i]) && CRpresence[[j]][i] == "1"){
      adjuv <- get_adjuv(date, code)
      CRpresence[i, c(code)] <- as.character(adjuv)
    }
  }
}


# Count all adjuvs

CRpresence$AM <- apply(CRpresence, 1, function(x) length(which(x=="AM")))
CRpresence$AF <- apply(CRpresence, 1, function(x) length(which(x=="AF")))
CRpresence$ASR <- CRpresence$AM/(CRpresence$AF+CRpresence$AM)
CRpresence$SM <- apply(CRpresence, 1, function(x) length(which(x=="SM")))
CRpresence$JF <- apply(CRpresence, 1, function(x) length(which(x=="JF")))
CRpresence$JM <- apply(CRpresence, 1, function(x) length(which(x=="JM")))
CRpresence$BB <- apply(CRpresence, 1, function(x) length(which(x=="BB")))

CRCompo <- CRpresence[,c("Date", "ASR", "AM", "AF", "SM", "JF", "JM", "BB")]


#### KUBU ####

# add StartKB & EndKB + check structure
LHKB$StartKB <- NA
LHKB$StartKB <- as.Date(LHKB$StartKB, format = "%d.%m.%Y")
LHKB$EndKB <- NA
LHKB$EndKB <- as.Date(LHKB$EndKB, format = "%d.%m.%Y")
str(LHKB)

# extract start date for all the KB individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHKB)) {
  
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB") {
    LHKB$StartKB[i] <- LHKB$DOB[i]} 
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB" & is.na(LHKB$DOB[i])) {
    LHKB$StartKB[i] <- LHKB$FirstRecorded[i]} 
  if (LHKB$ImmigrationGp1[i] == "KB" & !is.na(LHKB$DateImmigration1[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration1[i]} 
  if (LHKB$ImmigrationGp2[i] == "KB" & !is.na(LHKB$DateImmigration2[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration2[i]} 
  if (LHKB$ImmigrationGp3[i] == "KB" & !is.na(LHKB$DateImmigration3[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration3[i]} 
  if (LHKB$ImmigrationGp4[i] == "KB" & !is.na(LHKB$DateImmigration4[i])) {
    LHKB$StartKB[i] <- LHKB$DateImmigration4[i]} 
  
}

# extract end date for all the KB ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHKB)) {
  
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$DepartureNatalGp[i]} 
  if (!is.na(LHKB$ImmigrationGp1[i]) & (LHKB$ImmigrationGp1[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen1[i]}
  if (!is.na(LHKB$BirthGp[i]) & (LHKB$BirthGp[i]) == "KB" & is.na(LHKB$DepartureNatalGp[i])) {
    LHKB$EndKB[i] <- LHKB$LastSeen1[i]} 
  if (!is.na(LHKB$ImmigrationGp2[i]) & (LHKB$ImmigrationGp2[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen2[i]} 
  if (!is.na(LHKB$ImmigrationGp3[i]) & (LHKB$ImmigrationGp3[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen3[i]} 
  if (!is.na(LHKB$ImmigrationGp4[i]) & (LHKB$ImmigrationGp4[i]) == "KB") {
    LHKB$EndKB[i] <- LHKB$LastSeen4[i]}
  if (!is.na(LHKB$PresentGp[i]) & LHKB$PresentGp[i] == "KB" & is.na(LHKB$EndKB[i]))  {LHKB$EndKB[i] <- LHKB$LastSeen[i]} 
  
}


# Create a new table indicating yearly presence of ind in a group
KB <- LHKB[,c(1,2,33:37)]
#KB[,c(6:19)] <- NA
#colnames(KB) <- c("Individidual","Sex","DOB","StartKB", "EndKB","TotalDurationKB","A2010","A2011","A2012","A2013","A2014","A2015","A2016","A2017","A2018","A2019","A2020","A2021", "A2022")
head(KB)

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
KB$EndKB[is.na(KB$EndKB)] <- Sys.Date()

# Create a table with date on the y-axes - #AM #AF #SM #JM #JF #BB on x-axes

adjuv <- function(Sex, Age) {
  i = 1
  Adjuv <- list(nrow(KB))
  for(i in 1:nrow(KB))
    if (is.na(Sex[i]) | is.na(Age[i])) {
      Adjuv[i] <- NA
    } else {
      if(Sex[i] == "F" && Age[i] >= 4){
        Adjuv[i] <- "AF"
      } else {
        if(Sex[i] == "F" && Age[i] < 4 && Age[i] >= 0){
          Adjuv[i] <- "JF"
        } else {
          if(Sex[i] == "M" && Age[i] <= 3 && Age[i] >= 0){
            Adjuv[i] <- "JM"
          } else {
            if(Sex[i] == "M" && Age[i] == 4){
              Adjuv[i] <- "SM"
            } else {
              if(Sex[i] == "M" && Age[i] > 4){
                Adjuv[i] <- "AM"
              } else {
                if(Age[i] == -1){
                  Adjuv[i] <- "BB"
                } else { 
                  Adjuv[i] <- NA
                }
              }
            }
          }
        }
      }
      
    }
  return(Adjuv)
}                          


KB$AJ2020 <- as.factor(do.call(rbind, adjuv(KB$Sex, KB$A2020)))
KB$AJ2021 <- as.factor(do.call(rbind, adjuv(KB$Sex, KB$A2021)))
KB$AJ2022 <- as.factor(do.call(rbind, adjuv(KB$Sex, KB$A2022)))


# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = KB$Code,
  Start = KB$StartKB,
  End = KB$EndKB
)


# Create the matrix
KBpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

#str(KBpresence)
# Function to replace 1 with adjuv

get_adjuv <- function(date, code){
  if (date == 2020){
    adjuv <- KB[KB[,1] == code,]$AJ2020}
  if (date == 2021){
    adjuv <- KB[KB[,1] == code,]$AJ2021}
  if (date == 2022){
    adjuv <- KB[KB[,1] == code,]$AJ2022}
  
  return(adjuv)
}

codes <- colnames(KBpresence)[1:ncol(KBpresence)]

for (i in (1:nrow(KBpresence))){
  date <- as.numeric(format(as.POSIXct(KBpresence$Date[i], format = "%Y-%m-%d"), "%Y"))
  for (j in (2:ncol(KBpresence))) {
    code = codes[j]
    KBpresence[[j]] <- as.character(KBpresence[[j]])
    if(!is.na(KBpresence[[j]][i]) && KBpresence[[j]][i] == "1"){
      adjuv <- get_adjuv(date, code)
      KBpresence[i, c(code)] <- as.character(adjuv)
    }
  }
}


# Count all adjuvs

KBpresence$AM <- apply(KBpresence, 1, function(x) length(which(x=="AM")))
KBpresence$AF <- apply(KBpresence, 1, function(x) length(which(x=="AF")))
KBpresence$ASR <- KBpresence$AM/(KBpresence$AF+KBpresence$AM)
KBpresence$SM <- apply(KBpresence, 1, function(x) length(which(x=="SM")))
KBpresence$JF <- apply(KBpresence, 1, function(x) length(which(x=="JF")))
KBpresence$JM <- apply(KBpresence, 1, function(x) length(which(x=="JM")))
KBpresence$BB <- apply(KBpresence, 1, function(x) length(which(x=="BB")))

KBCompo <- KBpresence[,c("Date", "ASR", "AM", "AF", "SM", "JF", "JM", "BB")]



#### LEMON TREE ####

# add StartLT & EndLT + check structure
LHLT$StartLT <- NA
LHLT$StartLT <- as.Date(LHLT$StartLT, format = "%d.%m.%Y")
LHLT$EndLT <- NA
LHLT$EndLT <- as.Date(LHLT$EndLT, format = "%d.%m.%Y")
str(LHLT)

# extract start date for all the LT individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHLT)) {
  
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT") {
    LHLT$StartLT[i] <- LHLT$DOB[i]} 
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT" & is.na(LHLT$DOB[i])) {
    LHLT$StartLT[i] <- LHLT$FirstRecorded[i]} 
  if (LHLT$ImmigrationGp1[i] == "LT" & !is.na(LHLT$DateImmigration1[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration1[i]} 
  if (LHLT$ImmigrationGp2[i] == "LT" & !is.na(LHLT$DateImmigration2[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration2[i]} 
  if (LHLT$ImmigrationGp3[i] == "LT" & !is.na(LHLT$DateImmigration3[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration3[i]} 
  if (LHLT$ImmigrationGp4[i] == "LT" & !is.na(LHLT$DateImmigration4[i])) {
    LHLT$StartLT[i] <- LHLT$DateImmigration4[i]} 
  
}

# extract end date for all the LT ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHLT)) {
  
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$DepartureNatalGp[i]} 
  if (!is.na(LHLT$ImmigrationGp1[i]) & (LHLT$ImmigrationGp1[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen1[i]}
  if (!is.na(LHLT$BirthGp[i]) & (LHLT$BirthGp[i]) == "LT" & is.na(LHLT$DepartureNatalGp[i])) {
    LHLT$EndLT[i] <- LHLT$LastSeen1[i]} 
  if (!is.na(LHLT$ImmigrationGp2[i]) & (LHLT$ImmigrationGp2[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen2[i]} 
  if (!is.na(LHLT$ImmigrationGp3[i]) & (LHLT$ImmigrationGp3[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen3[i]} 
  if (!is.na(LHLT$ImmigrationGp4[i]) & (LHLT$ImmigrationGp4[i]) == "LT") {
    LHLT$EndLT[i] <- LHLT$LastSeen4[i]}
  if (!is.na(LHLT$PresentGp[i]) & LHLT$PresentGp[i] == "LT" & is.na(LHLT$EndLT[i]))  {LHLT$EndLT[i] <- LHLT$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
LT <- LHLT[,c(1,2,33:37)]
#LT[,c(6:19)] <- NA
#colnames(LT) <- c("Individidual","Sex","DOB","StartLT", "EndLT","TotalDurationLT","A2010","A2011","A2012","A2013","A2014","A2015","A2016","A2017","A2018","A2019","A2020","A2021", "A2022")
head(LT)

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
LT$EndLT[is.na(LT$EndLT)] <- Sys.Date()

# Create a table with date on the y-axes - #AM #AF #SM #JM #JF #BB on x-axes

adjuv <- function(Sex, Age) {
  i = 1
  Adjuv <- list(nrow(LT))
  for(i in 1:nrow(LT))
    if (is.na(Sex[i]) | is.na(Age[i])) {
      Adjuv[i] <- NA
    } else {
      if(Sex[i] == "F" && Age[i] >= 4){
        Adjuv[i] <- "AF"
      } else {
        if(Sex[i] == "F" && Age[i] < 4 && Age[i] >= 0){
          Adjuv[i] <- "JF"
        } else {
          if(Sex[i] == "M" && Age[i] <= 3 && Age[i] >= 0){
            Adjuv[i] <- "JM"
          } else {
            if(Sex[i] == "M" && Age[i] == 4){
              Adjuv[i] <- "SM"
            } else {
              if(Sex[i] == "M" && Age[i] > 4){
                Adjuv[i] <- "AM"
              } else {
                if(Age[i] == -1){
                  Adjuv[i] <- "BB"
                } else { 
                  Adjuv[i] <- NA
                }
              }
            }
          }
        }
      }
      
    }
  return(Adjuv)
}                          


LT$AJ2020 <- as.factor(do.call(rbind, adjuv(LT$Sex, LT$A2020)))
LT$AJ2021 <- as.factor(do.call(rbind, adjuv(LT$Sex, LT$A2021)))
LT$AJ2022 <- as.factor(do.call(rbind, adjuv(LT$Sex, LT$A2022)))


# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = LT$Code,
  Start = LT$StartLT,
  End = LT$EndLT
)


# Create the matrix
LTpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

LTpresence <- LTpresence[,!("V1")]
#str(LTpresence)
# Function to replace 1 with adjuv

get_adjuv <- function(date, code){
  if (date == 2020){
    adjuv <- LT[LT[,1] == code,]$AJ2020}
  if (date == 2021){
    adjuv <- LT[LT[,1] == code,]$AJ2021}
  if (date == 2022){
    adjuv <- LT[LT[,1] == code,]$AJ2022}
  
  return(adjuv)
}

codes <- colnames(LTpresence)[1:ncol(LTpresence)]

for (i in (1:nrow(LTpresence))){
  date <- as.numeric(format(as.POSIXct(LTpresence$Date[i], format = "%Y-%m-%d"), "%Y"))
  for (j in (2:ncol(LTpresence))) {
    code = codes[j]
    LTpresence[[j]] <- as.character(LTpresence[[j]])
    if(!is.na(LTpresence[[j]][i]) && LTpresence[[j]][i] == "1"){
      adjuv <- get_adjuv(date, code)
      LTpresence[i, c(code)] <- as.character(adjuv)
    }
  }
}


# Count all adjuvs

LTpresence$AM <- apply(LTpresence, 1, function(x) length(which(x=="AM")))
LTpresence$AF <- apply(LTpresence, 1, function(x) length(which(x=="AF")))
LTpresence$ASR <- LTpresence$AM/(LTpresence$AF+LTpresence$AM)
LTpresence$SM <- apply(LTpresence, 1, function(x) length(which(x=="SM")))
LTpresence$JF <- apply(LTpresence, 1, function(x) length(which(x=="JF")))
LTpresence$JM <- apply(LTpresence, 1, function(x) length(which(x=="JM")))
LTpresence$BB <- apply(LTpresence, 1, function(x) length(which(x=="BB")))

LTCompo <- LTpresence[,c("Date", "ASR", "AM", "AF", "SM", "JF", "JM", "BB")]

#### NOHA ####

# add StartNH & EndNH + check structure
LHNH$StartNH <- NA
LHNH$StartNH <- as.Date(LHNH$StartNH, format = "%d.%m.%Y")
LHNH$EndNH <- NA
LHNH$EndNH <- as.Date(LHNH$EndNH, format = "%d.%m.%Y")
str(LHNH)

# extract start date for all the NH individuals (birth + immigration), but several possible start dates (DOB,First Recorded, DateImmigration1 + 2 + 3 + 4)

for(i in 1:nrow(LHNH)) {
  
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH") {
    LHNH$StartNH[i] <- LHNH$DOB[i]} 
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH" & is.na(LHNH$DOB[i])) {
    LHNH$StartNH[i] <- LHNH$FirstRecorded[i]} 
  if (LHNH$ImmigrationGp1[i] == "NH" & !is.na(LHNH$DateImmigration1[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration1[i]} 
  if (LHNH$ImmigrationGp2[i] == "NH" & !is.na(LHNH$DateImmigration2[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration2[i]} 
  if (LHNH$ImmigrationGp3[i] == "NH" & !is.na(LHNH$DateImmigration3[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration3[i]} 
  if (LHNH$ImmigrationGp4[i] == "NH" & !is.na(LHNH$DateImmigration4[i])) {
    LHNH$StartNH[i] <- LHNH$DateImmigration4[i]} 
  
}

# extract end date for all the NH ind (death + emigration), but several possible end dates (DepartureNatalGp, LastSeen + 1 + 2 + 3 + 4)

for(i in 1:nrow(LHNH)) {
  
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$DepartureNatalGp[i]} 
  if (!is.na(LHNH$ImmigrationGp1[i]) & (LHNH$ImmigrationGp1[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen1[i]}
  if (!is.na(LHNH$BirthGp[i]) & (LHNH$BirthGp[i]) == "NH" & is.na(LHNH$DepartureNatalGp[i])) {
    LHNH$EndNH[i] <- LHNH$LastSeen1[i]} 
  if (!is.na(LHNH$ImmigrationGp2[i]) & (LHNH$ImmigrationGp2[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen2[i]} 
  if (!is.na(LHNH$ImmigrationGp3[i]) & (LHNH$ImmigrationGp3[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen3[i]} 
  if (!is.na(LHNH$ImmigrationGp4[i]) & (LHNH$ImmigrationGp4[i]) == "NH") {
    LHNH$EndNH[i] <- LHNH$LastSeen4[i]}
  if (!is.na(LHNH$PresentGp[i]) & LHNH$PresentGp[i] == "NH" & is.na(LHNH$EndNH[i]))  {LHNH$EndNH[i] <- LHNH$LastSeen[i]} 
  
}

# Create a new table indicating yearly presence of ind in a group
NH <- LHNH[,c(1,2,33:37)]
#NH[,c(6:19)] <- NA
#colnames(NH) <- c("Individidual","Sex","DOB","StartNH", "EndNH","TotalDurationNH","A2010","A2011","A2012","A2013","A2014","A2015","A2016","A2017","A2018","A2019","A2020","A2021", "A2022")
head(NH)

# Replace NA's in the End table with the most recent date (since individuals that are still in the group will have NA for their end date)
NH$EndNH[is.na(NH$EndNH)] <- Sys.Date()

# Create a table with date on the y-axes - #AM #AF #SM #JM #JF #BB on x-axes

adjuv <- function(Sex, Age) {
  i = 1
  Adjuv <- list(nrow(NH))
  for(i in 1:nrow(NH))
    if (is.na(Sex[i]) | is.na(Age[i])) {
      Adjuv[i] <- NA
    } else {
      if(Sex[i] == "F" && Age[i] >= 4){
        Adjuv[i] <- "AF"
      } else {
        if(Sex[i] == "F" && Age[i] < 4 && Age[i] >= 0){
          Adjuv[i] <- "JF"
        } else {
          if(Sex[i] == "M" && Age[i] <= 3 && Age[i] >= 0){
            Adjuv[i] <- "JM"
          } else {
            if(Sex[i] == "M" && Age[i] == 4){
              Adjuv[i] <- "SM"
            } else {
              if(Sex[i] == "M" && Age[i] > 4){
                Adjuv[i] <- "AM"
              } else {
                if(Age[i] == -1){
                  Adjuv[i] <- "BB"
                } else { 
                  Adjuv[i] <- NA
                }
              }
            }
          }
        }
      }
      
    }
  return(Adjuv)
}                          


NH$AJ2020 <- as.factor(do.call(rbind, adjuv(NH$Sex, NH$A2020)))
NH$AJ2021 <- as.factor(do.call(rbind, adjuv(NH$Sex, NH$A2021)))
NH$AJ2022 <- as.factor(do.call(rbind, adjuv(NH$Sex, NH$A2022)))


# Create a dataframe with just ID, Start and End date
df <- tibble(
  ID = NH$Code,
  Start = NH$StartNH,
  End = NH$EndNH
)


# Create the matrix
NHpresence <- dcast(
  setDT(df)[setDT(dfDate), .(Date, ID), on = .(Start <= Date, End >= Date)],
  Date ~ ID,
  fun.aggregate = length
)

NHpresence <- NHpresence[,!("V1")]
#str(NHpresence)
# Function to replace 1 with adjuv

get_adjuv <- function(date, code){
  if (date == 2020){
    adjuv <- NH[NH[,1] == code,]$AJ2020}
  if (date == 2021){
    adjuv <- NH[NH[,1] == code,]$AJ2021}
  if (date == 2022){
    adjuv <- NH[NH[,1] == code,]$AJ2022}
  
  return(adjuv)
}

codes <- colnames(NHpresence)[1:ncol(NHpresence)]

for (i in (1:nrow(NHpresence))){
  date <- as.numeric(format(as.POSIXct(NHpresence$Date[i], format = "%Y-%m-%d"), "%Y"))
  for (j in (2:ncol(NHpresence))) {
    code = codes[j]
    NHpresence[[j]] <- as.character(NHpresence[[j]])
    if(!is.na(NHpresence[[j]][i]) && NHpresence[[j]][i] == "1"){
      adjuv <- get_adjuv(date, code)
      NHpresence[i, c(code)] <- as.character(adjuv)
    }
  }
}


# Count all adjuvs

NHpresence$AM <- apply(NHpresence, 1, function(x) length(which(x=="AM")))
NHpresence$AF <- apply(NHpresence, 1, function(x) length(which(x=="AF")))
NHpresence$ASR <- NHpresence$AM/(NHpresence$AF+NHpresence$AM)
NHpresence$SM <- apply(NHpresence, 1, function(x) length(which(x=="SM")))
NHpresence$JF <- apply(NHpresence, 1, function(x) length(which(x=="JF")))
NHpresence$JM <- apply(NHpresence, 1, function(x) length(which(x=="JM")))
NHpresence$BB <- apply(NHpresence, 1, function(x) length(which(x=="BB")))

NHCompo <- NHpresence[,c("Date", "ASR", "AM", "AF", "SM", "JF", "JM", "BB")]

## Combine all groups ####

join <- dplyr::left_join(AKCompo, BDCompo, by = "Date")
join <- dplyr::left_join(join, KBCompo, by = "Date")
join <- dplyr::left_join(join, LTCompo, by = "Date")
join <- dplyr::left_join(join, NHCompo, by = "Date")

grp.compo <- join
colnames(grp.compo) <- c("Date", "ASR.ak", "AM.ak", "AF.ak", "SM.ak", "JF.ak", "JM.ak", "BB.ak",
                         "ASR.bd", "AM.bd", "AF.bd", "SM.bd", "JF.bd", "JM.bd", "BB.bd",
                         "ASR.kb", "AM.kb", "AF.kb", "SM.kb", "JF.kb", "JM.kb", "BB.kb",
                         "ASR.lt", "AM.lt", "AF.lt", "SM.lt", "JF.lt", "JM.lt", "BB.lt",
                         "ASR.nh", "AM.nh", "AF.nh", "SM.nh", "JF.nh", "JM.nh", "BB.nh")

ak1 <- dplyr::left_join(ak, AKCompo, by = "Date")
bd1 <- dplyr::left_join(bd, BDCompo, by = "Date")
kb1 <- dplyr::left_join(kb, KBCompo, by = "Date")
lt1 <- dplyr::left_join(lt, LTCompo, by = "Date")
nh1 <- dplyr::left_join(nh, NHCompo, by = "Date")


#### Check assumptions Elo package ####

library(EloRating)
library(lubridate)
library(tidyverse)

d <- read.csv("/Users/mariagranell/Repositories/master_students/josies_scripts/Agonistic.CSV", header=TRUE, na.strings=c(""," ","NA"))

# Split up Day, Month, Year 

#d$Day <- format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%d")
#d$Month <- format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%m")
#d$Year <- format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%Y")
#d$Date <- as.Date(format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))
d$Date <- dmy(d$Date)

#d$Day <- as.integer(d$Day)
#d$Month <- as.integer(d$Month)
#d$Year <- as.integer(d$Year)

# Exclude dates from before 2016 (very little)
#d <- d[!d$Year<2016,]


## Assumptions:
# File is ordered by date
# There are no empty cells
# IDs occur more than once
# Loser ID is different from winner ID


# Remove first column

#d <- d[,c(2:11)]

# Order by date
d <- dplyr::arrange(d, Date)

# Omit NA's
df <- d
d <- df
d <-d %>% select(Date,Time,Group,Data,Context,IDIndividual1,BehaviourIndiv1,IDIndividual2,BehaviourIndiv2)
d[1884,5] <- "Unknown"
d <- na.omit(d)

# set everything to lower case
#d$AggressorBehaviour <- tolower(d$AggressorBehaviour)
#d$VictimBehaviour <- tolower(d$VictimBehaviour)


#### Winner/loser ####
## Written by Stephanie Mercier ##
# Omit NA's 
#d <- na.omit(d)

#d <- subset(d, d$Year.x > 2020)

# In order to add the social rank of initiators & targets and thus get their social rank differences, 
# we first need to create the "fight.data" from data which include all agonistic interactions in which 
# there is a clear winner (defined as the individual being the most aggressive, i.e. who used the most 
# intense aggressive behaviour: 
# 1. approach -> approach.data, 
# 2. aggressive -> st,at,vo,ag+tp/dp, 
# 3. chase -> ch & 
# 4. physical contact -> bi.gb.hi.fi) 
# and a clear loser (defined as the individual showing the most submissive behaviours and/or 
# ending the conflict by moving away from the opponent -> rt,av,fl,le,re,ja,cr). 

### Fight data ###
#agg.behaviours <- c('ac', 'ag', 'ap','at','avo', 'dp', 'tp', 'st', 'ch', 'bi', 'gb', 'hi', 'fi', 'hh', 'so', 'fm', 'ap0', 'ap2', 'ap5', 'ap10', 'su', 'fh', 'wb', 'wb0', 'wb2', 'wb5', 'sf', 'hb', 'bd')


# select all observations in which we find at least one of the following aggressive behaviours
#fight.data <- d[union(
#  grep(paste(agg.behaviours, collapse = '|'), d$AggressorBehaviour),
#  grep(paste(agg.behaviours, collapse = '|'), d$VictimBehaviour)), ]
#fight.data <- d

rownames(fight.data) <- NULL
summary(fight.data)


### Write a function to decide for each obs who is the winner (decide.win) based on categories of behaviours specific to vic or agg
# Victim = individual ending up the conflict being the loser, thus considered as the most submissive one
ret_beh <- c('fl', 'rt', 're', 'av', 'le', 'ja', 'cr', 'ss', 'gu')

# Aggressor = from the least (cat_1) to the most aggressive behaviours (cat_3), the animal performing the most intense aggressive behaviour is the winner
agg_cat <- list(cat_3=c('bi', 'gb', 'hi', 'fi', 'hh', 'so', 'fm'), 
                cat_2='ch', cat_1=c('ac', 'at', 'dp', 'tp', 'st', 'su', 'fh', 'sf', 'hb', 'bd'))


# write the function which defines winner/loser by looking at which individual ends the conflict and/or 
# which one performs the most aggressive behaviours
decide.win <- function(beh_x, beh_y){
  x <- 0
  y <- 0
  
  for(beh_ in ret_beh){
    x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
    y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
  }
  if(x < y) return(1)
  if(y < x) return(2)
  
  for(cat_ in agg_cat){
    x <- 0
    y <- 0
    
    for(beh_ in cat_){
      x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
      y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
    }
    
    if(x > y) return(1)
    if(y > x) return(2)
  }
  return(0)
}

fight.data <- d

# add win/lose to fight.data
fight.data$win_lose <- mapply(FUN = decide.win, 
                              fight.data$BehaviourIndiv1,
                              fight.data$BehaviourIndiv2)

# write the function which allows to calculate for each sequence of obs between two individuals, 
# how many aggressive behaviours of the different severity each of them performed 
# (starting with the most intense one, i.e., involving physical contact), and as soon as one of the individuals
# has a greater number of aggressive behaviours performed, we declare this individual as the winner 
# (ignoring the rest of the sequence, but making sure that this one is not the individual also ending up the conflict by moving away...)

compute.wins <- function(l.data, ind_x, ind_y){
  
  fight.vec <- l.data$win_lose
  
  return(c(sum(c(fight.vec[l.data$Aggressor==ind_x]==1,
                 fight.vec[l.data$Victim==ind_x]==2)),
           sum(c(fight.vec[l.data$Aggressor==ind_y]==1,
                 fight.vec[l.data$Victim==ind_y]==2))))
}

# previously used to calculate dyadic rank -> not used anymore as looking at ISI's scores! but can use it to "clean" fight.data 
# as Aggressors actually not always real aggressors!! So use the two previously written functions to decide for 
# each observation of our fight.data, which individual can be considered as the winner using all the conflicts 
# between these two individuals that occurred within the last year (365days): 
#fight.data$AggScore <- NA
#fight.data$VicScore <- NA

## Stability of the hierarchy
# to decide how many days before interaction we should use
# N = 30 days -> No_clear_winner N = 3401
# N = 60 days -> No_clear_winner N = 2989
# N = 90 days -> No_clear_winner N = 2703
# N = 120 days -> No_clear_winner N = 2515
# N = 150 days -> No_clear_winner N = 2330
# N = 180 days -> No_clear_winner N = 2183
# N = 365 days -> No_clear_winner N = 1738 # hierarchy btw females really stable -> fine to use 1 year!

#i=1
#for(i in 1:nrow(fight.data)){
#  print(i) # to see how fast/good the script is performing
#  min_date <- fight.data$Date[i]-365
#   x <- as.character(fight.data$Aggressor[i])
#   y <- as.character(fight.data$Victim[i])
#   win_lose <- compute.wins(fight.data[fight.data$Aggressor %in% c(x,y) &
#                                         fight.data$Victim %in% c(x,y) &
#                                         fight.data$Date >= min_date &
#                                         fight.data$Date < fight.data$Date[i], ],
#                            x,
#                            y)
#   fight.data$AggScore[i] <- win_lose[1]
#   fight.data$VicScore[i] <- win_lose[2]
# }

seq.data <- fight.data

# define winner & loser using win_lose as 1 means that Aggressor is the most aggressive ind so the winner, 
# whereas 2 means that Victim is the most aggressive ind and thus the winner!
seq.data$winner <- NA
seq.data$BehaviourW <- NA
seq.data$loser <- NA
seq.data$BehaviourL <- NA

i=1
for(i in 1:nrow(seq.data)){
  if (seq.data$win_lose[i]=="1") seq.data$winner[i] <- as.character(seq.data$IDIndividual1[i])
  if (seq.data$win_lose[i]=="1") seq.data$BehaviourW[i] <- as.character(seq.data$BehaviourIndiv1[i])
  if (seq.data$win_lose[i]=="1") seq.data$loser[i] <- as.character(seq.data$IDIndividual2[i])
  if (seq.data$win_lose[i]=="1") seq.data$BehaviourL[i] <- as.character(seq.data$BehaviourIndiv2[i])
  if (seq.data$win_lose[i]=="2") seq.data$winner[i] <- as.character(seq.data$IDIndividual2[i])
  if (seq.data$win_lose[i]=="2") seq.data$BehaviourW[i] <- as.character(seq.data$BehaviourIndiv2[i])
  if (seq.data$win_lose[i]=="2") seq.data$loser[i] <- as.character(seq.data$IDIndividual1[i])
  if (seq.data$win_lose[i]=="2") seq.data$BehaviourL[i] <- as.character(seq.data$BehaviourIndiv1[i])
}

# write undecided interaction -> all data where win_lose=0 as it corresponds to interactions with no clear winner/loser
seq.data$Draw <- FALSE # when decided interaction in which clear winner/loser is known 
NotClear <- which(seq.data$win_lose=="0")
seq.data[NotClear,"Draw"] <- TRUE
table(seq.data$Draw) 

#rownames(seq.data) <- NULL
#seq.data <- seq.data[,-c(12,13)]


# add intensity: mild, chase, severe
seq.data$intensity <- NA
mild <- which(grepl("ap|ac|ag|at|dp|tp|st|vo|gu|sc|ap0|ap2|ap10|hb",seq.data$BehaviourW)=="TRUE")
seq.data[mild,"intensity"] <- "mild"
chase <- which(grepl("ch",seq.data$BehaviourW)=="TRUE")
seq.data[chase,"intensity"] <- "chase"
severe <- which(grepl("bi|gb|hi|fi|hh|so|fm",seq.data$BehaviourW)=="TRUE")
seq.data[severe,"intensity"] <- "severe"

table(seq.data$intensity)
1542+11149+2452 # 15143
# Exclude the interactions that had no clear winners
X <- which(is.na(seq.data$intensity))
seq.data <- seq.data[-X,]

rownames(seq.data) <- NULL


seq.data$winner <- as.factor(seq.data$winner)
nlevels(seq.data$winner)
seq.data$loser <- as.factor(seq.data$loser)
nlevels(seq.data$loser)

d <- seq.data
#d$Date <- as.Date(d$Date)
str(d)

## Link life history to individuals ####

#d$Day <- format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%d")
#d$Month <- format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%m")
#d$Year <- format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%Y")
#d$Date <- as.Date(format(as.POSIXct(d$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))

#d$Day <- as.integer(d$Day)
#d$Month <- as.integer(d$Month)
#d$Year <- as.integer(d$Year)

#colnames(d)[4] <- "winner"
#colnames(d)[5] <- "WinnerBehaviour"
#colnames(d)[6] <- "loser"
#colnames(d)[7] <- "LoserBehaviour"
d <- d %>% rename(WinnerBehaviour = BehaviourW, LoserBehaviour = BehaviourL)


lh <- read.csv("/Users/mariagranell/Repositories/data_summaries/life_history/IVP_Lifehistory_020822.csv", header = T)
#View(lh)
str(lh)

# Create a column with the year the individual was either born (DOB) or first seen

# DOB has two types of input: Year and Day/Month/Year. We want to use both these types of input in a new column
# We therefore first make two columns: DOB2 with the input that is just Year and DOB3 with the input that is D/M/Y
lh$YearDOB2 <- as.numeric(format(as.POSIXct(lh$DOB, format = "%Y"), "%Y"))
lh$YearDOB3 <- as.numeric(format(as.POSIXct(lh$DOB, format = "%d/%m/%Y"), "%Y"))

# DOB2 also has input of the D/M/Y format. Delete these
lh$YearDOB2[lh$YearDOB2 < 1000] <- NA

# Only select the lowest value of DOB2 and DOB3 into new column DOB
lh$YearDOB <- apply(X = lh[,29:30],                     
                    MARGIN = 1,                           
                    FUN = function(x) min(x,na.rm = T) 
)
# This gives you a warning for every row where R encounters an NA
# You can therefore ignore this warning
str(lh)

# Apply function gives Inf as output for rows that had NA. Get rid of these by selecting only values below 3000 (since it's 2022 ;) 
lh$YearDOB[lh$YearDOB > 3000] <- NA

# Create a column with the year the individual was first recorded
lh$YearFR <- as.numeric(format(as.POSIXct(lh$FirstRecorded, format = "%d/%m/%Y"), "%Y"))

# New males that immigrate are already adults when they disperse. Their year of birth is therefore at least 4 years before first recorded
#lh$DateImmigration1 <- as.numeric(lh$DateImmigration1)
i = 1
lh$YearBO <- NA

for(i in 1:nrow(lh)) {
  if(is.na(lh$YearDOB[i])){
    if(is.na(lh$DateImmigration1[i])){
      lh$YearBO[i] <- NA
    } else {
      lh$YearBO[i] <- lh$YearFR[i] - 4
    }
  } else {
    lh$YearBO[i] <- lh$YearDOB[i]
  }
}


# Create a column with the year the individual was first seen or when it was born (first recording of the individual)
lh$Year <- apply(X = lh[,31:33],                     
                 MARGIN = 1,                           
                 FUN = function(x) min(x,na.rm = T) 
)
# Again, everytime R encounters an NA it returns Inf. You can ignore this
str(lh)

d <- d[,c(1:3,8:10,12:17)]

d$Year <- as.numeric(d$Year)
lh$Individual <- as.character(lh$Individual)
lh$Code <- as.character(lh$Code)
d$winner <- as.character(d$winner)

## Create column with AggressorAge, AggressorSex, VictimAge, VictimSex

# To join dataframes you need a column with the same name and value
lh$winner <- lh$Code
lh$loser <- lh$Code

# Only select the columns of interest
Agg <- lh[,c(5,34,35)]
Vic <- lh[,c(5,34,36)]

# join Aggressor dataframe with conflict dataframe
join <- dplyr::left_join(d, Agg, by = "winner", multiple = "all")

# calculate the age of the aggressor in the year of the interaction
join$AggressorAge <- join$Year.x - join$Year.y

# rename column with sex of the aggressor
colnames(join)[13] <- "AggressorSex"

# join Victim dataframe with conflict dataframe
x <- dplyr::left_join(join, Vic, by = "loser", multiple = "all")

# calculate age of victim in year of interaction
x$VictimAge <- x$Year.x - x$Year

# rename column with sex of the victim
colnames(x)[16] <- "VictimSex"


# tidy dataframe and only select columns of interest
d <- x[,c(1:3,7:10,12,13,15,16,18)]

# Omit NA's ####

d$winner[d$winner == ""] <- NA
d$BehaviourW[d$BehaviourW == ""] <- NA
d$loser[d$loser == ""] <- NA
d$AggressorSex[d$AggressorSex == ""] <- NA
d$AggressorAge[d$AggressorAge == ""] <- NA
d$VictimSex[d$VictimSex == ""] <- NA
d$VictimAge[d$VictimAge == ""] <- NA
d$Group[d$Group == ""] <- NA

d <- na.omit(d)

## Split into groups ####

# Divide into different groups

BD <- subset(d,d$Group%in%("Baie Dankie"))
NH <- subset(d,d$Group%in%("Noha"))
KB <- subset(d,d$Group%in%("Kubu"))
AK <- subset(d,d$Group%in%("Ankhase"))
LT <- subset(d,d$Group%in%("Lemon Tree"))
CR <- subset(d,d$Group%in%("Crossing"))
#IF <- subset(d,d$Group%in%("IFamily"))

BD <- BD[!BD$Date < "2021-01-01",]
NH <- NH[!NH$Date < "2021-01-01",]
AK <- AK[!AK$Date < "2021-01-01",]
KB <- KB[!KB$Date < "2021-01-01",]
LT <- LT[!LT$Date < "2021-01-01",]
CR <- CR[!CR$Date < "2021-01-01",]

# Create conflict file for function
write.csv(d, "C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Intra vs intergroup aggression/2021_now.csv")

#### Baie Dankie ELO-RATING ####

# To incorporate presence data
# First read the presence matrix

BDpres <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/BD2016-2022.csv", header=TRUE)
BDpres$Date <- as.Date(format(as.POSIXct(BDpres$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
#BDpres$Date  <- as.numeric(format(as.POSIXct(BDpres$Date, format = "%d/%m/%Y"), "%d/%m/%Y"))
colnames(BDpres)[1] <- "Delete"
colnames(BDpres)[3] <- "Delete"
BDpres <- BDpres[,!grepl("Delete",names(BDpres))]

# Check if data look good

str(BDpres)
head(BDpres)
tail(BDpres)

BDpres[is.na(BDpres)] <- 0

#### ALL BD INDIVIDUALS ####
## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=BD$winner, loser=BD$loser, Date=BD$Date, draw = NULL, presence=BDpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

# Errors:
# IDs in the data sequence but NOT in presence data: Alc, Bin, Ati, Nyan
# Exclude these individuals
BD <- BD[!(BD$winner == "Alc" | BD$winner == "Bin" | BD$winner == "Ati" | BD$winner == "Nyan"),]
BD <- BD[!(BD$loser == "Alc" | BD$loser == "Bin" | BD$loser == "Ati" | BD$loser == "Nyan"),]

# for ID (Kom, Ree, Mat) the first interaction occurred before first presence
# exclude these interactions?
# check!
# Temporary solution:
#BD <- BD[!(BD$winner == "Kom" | BD$winner == "Ree" | BD$winner == "Mat"),]
#BD <- BD[!(BD$loser == "Kom" | BD$loser == "Ree"| BD$loser == "Mat"),]
str(BD)

BD <- BD[!(BD$Aggressor == "Mat" & BD$Date <"2021-02-24"),]
BD <- BD[!(BD$Victim == "Mat" & BD$Date <"2021-02-24"),]

BD <- BD[!(BD$Aggressor == "Kom" & BD$Date <"2019-11-12"),]
BD <- BD[!(BD$Victim == "Kom" & BD$Date <"2019-11-12"),]

BD <- BD[!(BD$Aggressor == "Ree" & BD$Date <"2020-11-24"),]
BD <- BD[!(BD$Victim == "Ree" & BD$Date <"2020-11-24"),]

BD <- BD[!(BD$Aggressor == "Dok" & BD$Date < "2019-05-30"),]
BD <- BD[!(BD$Victim == "Dok" & BD$Date < "2019-05-30"),]

BD <- BD[!(BD$Aggressor == "Unw" & BD$Date < "2017-05-04"),]
BD <- BD[!(BD$Victim == "Unw" & BD$Date  < "2017-05-04"),]

BD <- BD[!(BD$Aggressor == "Hlo" & BD$Date < "2017-05-16"),]
BD <- BD[!(BD$Victim == "Hlo" & BD$Date < "2017-05-16"),]

BD <- BD[!(BD$Aggressor == "Nyo" & BD$Date < "2017-05-16"),]
BD <- BD[!(BD$Victim == "Nyo" & BD$Date < "2017-05-16"),]

BD <- BD[!(BD$Aggressor == "Van" & BD$Date < "2018-05-29"),]
BD <- BD[!(BD$Victim == "Van" & BD$Date < "2018-05-29"),]

BD <- BD[!(BD$Aggressor == "Kom" & BD$Date < "2020-07-09"),]
BD <- BD[!(BD$Victim == "Kom" & BD$Date < "2020-07-09"),]

#BDad <- subset(BD, BD$AggressorAge > 4)
#BDad <- subset(BDad, BDad$VictimAge > 4)

BDELO <- elo.seq(winner = BD$winner, loser=BD$loser, Date=BD$Date, presence = BDpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(BDELO)
eloplot(BDELO)

# Select specific IDs:
eloplot(BDELO, ids=c("Pro", "Pal", "Umb", "Ted", "Flu", "Dok", "Sey", "Nge", "Xia", "Mat", "Rhe", "Ouli", "Oort", "Obse", "Oerw", "Piep", "Asis", "Hond", "Heer", "Aapi", "Siel", "Gese", "Rede", "Puol", "Potj", "Pann", "Riss", "Sirk", "Sari", "Miel", "Numb", "Nurk")) # Poor Rissiepit :(
# Select a specific time period:
eloplot(BDELO,from="2021-11-30",to="2022-08-01")
# Specific IDs over a specific time period:
eloplot(BDELO, ids=c("Pro", "Pal", "Umb", "Ted", "Flu", "Dok", "Sey", "Nge", "Xia", "Mat", "Rhe", "Ouli", "Oort", "Obse", "Oerw", "Piep", "Asis", "Hond", "Heer", "Aapi", "Siel", "Gese", "Rede", "Puol", "Potj", "Pann", "Riss", "Sirk", "Sari", "Miel", "Numb", "Nurk"),  from = "2021-11-30", to = "2022-08-01")

# Males:
eloplot(BDELO, ids=c("Pro", "Pal", "Umb", "Ted", "Flu", "Dok", "Sey", "Nge", "Xia", "Nak", "Xin", "Pom", "Aan", "Hee", "Nuu"),  from = "2021-11-30", to = "2022-08-01")

# Females:
eloplot(BDELO, ids=c("Ouli", "Oort", "Obse", "Oerw", "Puol", "Potj", "Piep", "Asis", "Aapi", "Hond", "Heer", "Gese", "Rede", "Pann", "Riss", "Siel", "Sirk", "Sari", "Miel", "Numb", "Nurk", "Nooi", "Eina", "Enge"),  from = "2021-11-30", to = "2022-08-01")

# Get scores
extract_elo(BDELO) # Ouli above Pro

#to get elo score on a specific date
extract_elo(BDELO,"2020-12-18") # Rhe above Ouli!

#to get an average of the eloscore over a period -> here from 9.05.2014 until 17.10.2014 = 162 days later -> For the encounters taking focal data
extract_elo(BDELO, "2021-09-30", daterange=160) # Neu above Ouli

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(BDELO, "2020-05-09", daterange=162, standardize=TRUE) # Neu above Ouli


## BD Adult female dominance ####

BDaf <- subset(BD, BD$AggressorSex%in%("F"))
BDaf <- subset(BDaf, BDaf$VictimSex%in%("F"))
BDaf <- subset(BDaf, BDaf$AggressorAge >= 4)
BDaf <- subset(BDaf, BDaf$VictimAge >= 4)
# Select time period (optional)
#BDaf <- subset(BDaf, BDaf$Year.x > 2018)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=BDaf$winner, loser=BDaf$loser, Date=BDaf$Date, draw = NULL, presence=BDpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

BDafELO <- elo.seq(winner = BDaf$winner, loser=BDaf$loser, Date=BDaf$Date, presence = BDpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(BDafELO)
eloplot(BDafELO)
# Select specific IDs:
eloplot(BDafELO, ids=c("Ouli", "Oort", "Heer", "Aapi", "Siel", "Gese", "Rede", "Puol", "Potj", "Obse", "Riss")) # Poor Rissiepit :(
# Select a specific time period:
eloplot(BDafELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(BDafELO, ids=c("Ouli", "Obse", "Oort", "Oerw", "Puol", "Potj", "Piep", "Aapi", "Asis", "Eina", "Enge", "Heer", "Hond", "Gese", "Miel", "Siel", "Sirk", "Sari", "Pann", "Nooi", "Numb", "Nurk", "Rede", "Riss"), from="2021-11-30", to = "2022-05-17")

extract_elo(BDafELO)

#to get elo score on a specific date
extract_elo(BDafELO,"2022-02-30")

#to get an average of the eloscore over a period -> here from 9.05.2014 until 17.10.2014 = 162 days later -> For the encounters taking focal data
extract_elo(BDafELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(BDafELO, "2020-05-09", daterange=162, standardize=TRUE)


## WITHOUT PRESENCE 
# Check data
seqcheck(winner=BDaf$winner, loser=BDaf$loser, Date=BDaf$Date, draw=NULL, presence=NULL)

res <- elo.seq(winner=BDnow$winner, loser=BDnow$loser, Date=BDnow$Date, runcheck=F)

res

BDnow$winner
BDnow$loser
BDnow$Date

summary(res)
extract_elo(res)
extract_elo(res,"2021-08-01")

eloplot(res)
eloplot(res,from="2021-08-01",to="2021-11-08")

## BD Adult male dominance ####

BDam <- subset(BD, BD$AggressorSex%in%("M"))
BDam <- subset(BDam, BDam$VictimSex%in%("M"))
BDam <- subset(BDam, BDam$AggressorAge > 4)
BDam <- subset(BDam, BDam$VictimAge > 4)
#BDnow <- BDnow[!(BDnow$Aggressor == "Alc" | BDnow$Aggressor == "Bin" | BDnow$Aggressor == "Mat" | BDnow$Aggressor == "Dok"),]
#BDnow <- BDnow[!(BDnow$Victim == "Alc" | BDnow$Victim == "Bin" | BDnow$Victim == "Mat"| BDnow$Victim == "Dok"),]

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=BDam$winner, loser=BDam$loser, Date=BDam$Date, draw = NULL, presence=BDpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

BDamELO <- elo.seq(winner = BDam$winner, loser=BDam$loser, Date=BDam$Date, presence = BDpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(BDamELO)
eloplot(BDamELO)
# Select specific IDs:
eloplot(BDamELO, ids=c("Pro", "Pal", "Umb", "Ted", "Rhe", "Dok", "Sey", "Neu", "Obe")) 
# Select a specific time period:
eloplot(BDamELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(BDamELO, ids=c("Pro", "Pal", "Umb", "Ted", "Nge", "Dok", "Sey", "Xia", "Flu", "Kom", "Nak", "Hee", "Aan", "Pom", "Nuu"), from="2021-11-30",to="2022-05-17")

extract_elo(BDamELO)

#to get elo score on a specific date
extract_elo(BDamELO, "2022-03-30")

#to get an average of the eloscore over a period -> here from 9.05.2014 until 17.10.2014 = 162 days later -> For the encounters taking focal data
extract_elo(BDamELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(BDamELO, "2020-05-09", daterange=162, standardize=TRUE)

#### Ankhase ELO-RATING ####

# To incorporate presence data
# First read the presence matrix

AKpres <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/AK2016-2022.csv", header=TRUE)
AKpres$Date  <- as.Date(as.character(AKpres$Date))
colnames(AKpres)[1] <- "Delete"
AKpres <- AKpres[,!grepl("Delete",names(AKpres))]

# Check if data look good

str(AKpres)
head(AKpres)
tail(AKpres)

## ALL AK INDIVIDUALS ####
## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=AK$winner, loser=AK$loser, Date=AK$Date, draw = NULL, presence=AKpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (these ind apparantly didn't have any conflicts?)
# AK <- AK[!(AK$Aggressor == "Ncok"),]
# AK <- AK[!(AK$Victim == "Ncok"),]

AK <- AK[!(AK$Aggressor == "Yan" & AK$Date < "2020-03-04"),]
AK <- AK[!(AK$Victim == "Yan" & AK$Date < "2020-03-04"),]

AKELO <- elo.seq(winner = AK$winner, loser=AK$loser, Date=AK$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(AKELO)
eloplot(AKELO)

# Select specific IDs:
eloplot(AKELO, ids=c("Nak", "Buk", "Yan", "Nge", "Vla", "Tch", "Mat", "Ginq","Ndon", "Nkos", "Nyan", "Gubh", "Ghid", "Guba"))
# Select a specific time period:
eloplot(AKELO,from="2021-09-30",to="2022-08-01")
# Specific IDs over a specific time period:
eloplot(AKELO, ids=c("Nak", "Buk", "Yan", "Nge", "Vla", "Sho", "Tch", "Mat", "Ginq","Ndon", "Nkos", "Nyan", "Gubh", "Ghid", "Guba"),  from = "2021-09-30",to="2022-01-01")

# Get scores
extract_elo(AKELO)

# Males
eloplot(AKELO, ids=c("Nak", "Buk", "Yan", "Nge", "Vla", "Sho", "Tch"),  from = "2021-09-30",to="2022-08-01")

# Females
eloplot(AKELO, ids=c("Ginq", "Ghid", "Nkos", "Nyan", "Ndon", "Ndaw", "Gubh", "Godu"), from = "2021-09-30",to="2022-08-01")

#to get elo score on a specific date
extract_elo(AKELO) 

#to get an average of the eloscore over a period -> here from 9.05.2014 until 17.10.2014 = 162 days later -> For the encounters taking focal data
extract_elo(AKELO, "2020-05-09", daterange=162) # Neu above Ouli

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(AKELO, "2020-05-09", daterange=162, standardize=TRUE) # Neu above Ouli


## AK Adult female dominance ####

AKaf <- subset(AK, AK$AggressorSex%in%("F"))
AKaf <- subset(AKaf, AKaf$VictimSex%in%("F"))
AKaf <- subset(AKaf, AKaf$AggressorAge > 3)
AKaf <- subset(AKaf, AKaf$VictimAge > 3)
# Select time period (optional)
#AKaf <- subset(AKaf, AKaf$Year.x > 2018)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=AKaf$winner, loser=AKaf$loser, Date=AKaf$Date, draw = NULL, presence= AKpres)
# Warnings:
# There is 1 case in which loser ID equals winner ID (check!)
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

AKafELO <- elo.seq(winner = AKaf$winner, loser=AKaf$loser, Date=AKaf$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(AKafELO)
eloplot(AKafELO)
# Select specific IDs:
eloplot(AKafELO, ids=c("Ginq","Ndon", "Nkos", "Nyan", "Gubh", "Ghid", "Mamo", "Gugu")) 
# Select a specific time period:
eloplot(AKafELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(AKafELO, ids=c("Ginq","Ndon", "Nkos", "Nyan", "Gubh", "Ghid", "Mamo", "Gugu"), from="2020-01-01",to="2021-01-01")

extract_elo(AKafELO)

#to get elo score on a specific date
extract_elo(AKafELO,"2022-01-01")

#to get an average of the eloscore over a period -> here from 2020-05-09 to 2021-08-11
extract_elo(AKafELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(AKafELO, "2020-05-09", daterange=162, standardize=TRUE)


## AK Adult male dominance ####

AKam <- subset(AK, AK$AggressorSex%in%("M"))
AKam <- subset(AKam, AKam$VictimSex%in%("M"))
AKam <- subset(AKam, AKam$AggressorAge > 4)
AKam <- subset(AKam, AKam$VictimAge > 4)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=AKam$winner, loser=AKam$loser, Date=AKam$Date, draw = NULL, presence=AKpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

AKamELO <- elo.seq(winner = AKam$winner, loser=AKam$loser, Date=AKam$Date, presence = AKpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(AKamELO)
eloplot(AKamELO)
# Select specific IDs:
eloplot(AKamELO, ids=c("Boc", "Mat", "Ndi", "Yan", "Nge")) 
# Select a specific time period:
eloplot(AKamELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(AKamELO, ids=c("Boc", "Mat", "Ndi", "Yan", "Nge"), from="2020-09-30",to="2022-01-01")

extract_elo(AKamELO)

#to get elo score on a specific date
extract_elo(AKamELO,"2022-01-01")

#to get an average of the eloscore over a period 
extract_elo(AKamELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(AKamELO, "2020-05-09", daterange=162, standardize=TRUE)



#### Noha ELO-RATING ####

# To incorporate presence data
# First read the presence matrix

NHpres <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/NH2016-2022.csv", header=TRUE)
NHpres$Date <- as.Date(format(as.POSIXct(NHpres$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))
#NHpres$Date  <- as.Date(as.character(NHpres$Date))
colnames(NHpres)[1] <- "Delete"
NHpres <- NHpres[,!grepl("Delete",names(NHpres))]

# Check if data look good

str(NHpres)
head(NHpres)
tail(NHpres)

## ALL NH INDIVIDUALS ####
## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=NH$winner, loser=NH$loser, Date=NH$Date, draw = NULL, presence=NHpres)
# Warnings:
# There are 3 cases in which loser ID equals winner ID
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (these ind apparantly didn't have any conflicts?)

# The following IDs occur in the data sequence but NOT in the presence data ... ERROR
# War
NH <- NH[!(NH$winner == "Reno"),]
NH <- NH[!(NH$loser == "Reno"),]

NH <- NH[!(NH$Aggressor == "Cus" & NH$Date < "2019-05-21"),]
NH <- NH[!(NH$Victim == "Cus" & NH$Date < "2019-05-21"),]

NH <- NH[!(NH$Aggressor == "Can" & NH$Date < "2019-06-17"),]
NH <- NH[!(NH$Victim == "Can" & NH$Date < "2019-06-17"),]

NH <- NH[!(NH$Aggressor == "Fle" & NH$Date < "2022-01-18"),]
NH <- NH[!(NH$Victim == "Fle" & NH$Date < "2022-01-18"),]

#NHad <- subset(NH, NH$AggressorAge > 4)
#NHad <- subset(NHad, NHad$VictimAge > 4)

NHELO <- elo.seq(winner = NH$winner, loser=NH$loser, Date=NH$Date, presence = NHpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker
# ERROR: for ID (Cus, Can) the first interaction occurred before first presence

# Can and Fle actually have double presence in NH but the presence table only includes the latest. Follow up on this

summary(NHELO)
eloplot(NHELO)

# Select specific IDs:
eloplot(NHELO, ids=c("Bos", "Gri", "Kek", "Lif", "Gan", "Fle", "Pro", "Xia", "Beir","Bela", "Gaya", "Gran", "Guat", "Upps", "Xian"))
# Select a specific time period:
eloplot(NHELO,from="2021-09-30",to="2022-08-01")
# Specific IDs over a specific time period:
eloplot(NHELO, ids=c("Lif", "Gan", "Fle", "Vul", "Utr", "Tam", "Xia", "Gran", "Guat", "Xala", "Gaya", "Xian", "Upps", "Pret", "Prai", "Raba", "Bela", "Rioj"),  from = "2021-09-30", to = "2022-05-17")

# Males
eloplot(NHELO, ids=c("Lif", "Gan", "Fle", "Vul", "Utr", "Tam", "Xia", "Vry", "Erm", "Male3"),  from = "2021-09-30", to = "2022-08-01")

# Females
eloplot(NHELO, ids=c("Gran", "Guat", "Xala", "Gaya", "Xian", "Upps", "Pret", "Prai", "Raba", "Bela", "Rioj"),  from = "2021-09-30", to = "2022-08-01")

# Get scores
extract_elo(NHELO)

#to get elo score on a specific date
extract_elo(NHELO,"2020-12-18") 

#to get an average of the eloscore over a period 
extract_elo(NHELO, "2020-05-09", daterange=162) 

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHELO, "2020-05-09", daterange=162, standardize=TRUE) # Neu above Ouli


## NH Adult female dominance ####

NHaf <- subset(NH, NH$AggressorSex%in%("F"))
NHaf <- subset(NHaf, NHaf$VictimSex%in%("F"))
NHaf <- subset(NHaf, NHaf$AggressorAge > 3)
NHaf <- subset(NHaf, NHaf$VictimAge > 3)
# Select time period (optional)
#NHaf <- subset(NHaf, NHaf$Year.x > 2018)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=NHaf$winner, loser=NHaf$loser, Date=NHaf$Date, draw = NULL, presence= NHpres)
# Warnings:
# There is 1 case in which loser ID equals winner ID (check!)
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

NHafELO <- elo.seq(winner = NHaf$winner, loser=NHaf$loser, Date=NHaf$Date, presence = NHpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(NHafELO)
eloplot(NHafELO)
# Select specific IDs:
eloplot(NHafELO, ids=c("Bela", "Gaya", "Gran", "Guat", "Upps", "Xian", "Pret", "Prai", "Raba")) 
# Select a specific time period:
eloplot(NHafELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(NHafELO, ids=c("Bela", "Gaya", "Gran", "Guat", "Upps", "Xian", "Pret", "Prai", "Raba"), from="2020-01-01",to="2021-01-01")

extract_elo(NHafELO)

#to get elo score on a specific date
extract_elo(NHafELO,"2020-12-18")

#to get an average of the eloscore over a period -> here from 2020-05-09 to 2021-08-11
extract_elo(NHafELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHafELO, "2020-05-09", daterange=162, standardize=TRUE)


## NH Adult male dominance ####

NHam <- subset(NH, NH$AggressorSex%in%("M"))
NHam <- subset(NHam, NHam$VictimSex%in%("M"))
NHam <- subset(NHam, NHam$AggressorAge > 4)
NHam <- subset(NHam, NHam$VictimAge > 4)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=NHam$winner, loser=NHam$loser, Date=NHam$Date, draw = NULL, presence=NHpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

NHamELO <- elo.seq(winner = NHam$winner, loser=NHam$loser, Date=NHam$Date, presence = NHpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(NHamELO)
eloplot(NHamELO)
# Select specific IDs:
eloplot(NHamELO, ids=c("Lif", "Rey", "Wol", "Xia", "Yan", "Vul")) 
# Select a specific time period:
eloplot(NHamELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(NHamELO, ids=c("Lif", "Rey", "Wol", "Xia", "Yan", "Vul"), from="2020-01-01",to="2021-01-01")

extract_elo(NHamELO)
#to get elo score on a specific date
extract_elo(NHamELO,"2020-12-18")

#to get an average of the eloscore over a period 
extract_elo(NHamELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHamELO, "2020-05-09", daterange=162, standardize=TRUE)


#### Kubu ELO-RATING ####

# To incorporate presence data
# First read the presence matrix

KBpres <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/KB2016-2022.csv", header=TRUE)
KBpres$Date  <- as.Date(as.character(KBpres$Date))
colnames(KBpres)[1] <- "Delete"
KBpres <- KBpres[,!grepl("Delete",names(KBpres))]

# Check if data look good

str(KBpres)
head(KBpres)
tail(KBpres)

## ALL KB INDIVIDUALS ####
## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=KB$winner, loser=KB$loser, Date=KB$Date, draw = NULL, presence=KBpres)
# Warnings:
# There are 4 cases in which loser ID equals winner ID
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (these ind apparantly didn't have any conflicts?)
# The following IDs occur in the data sequence but NOT in the presence data ... ERROR
# Nug
KB <- KB[!(KB$winner == "Nug"),]
KB <- KB[!(KB$loser == "Nug"),]


KBELO <- elo.seq(winner = KB$winner, loser=KB$loser, Date=KB$Date, presence = KBpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(KBELO)
eloplot(KBELO)

# Select specific IDs:
eloplot(KBELO, ids=c("Lif", "Yan", "Oke", "Tev", "Arn", "Eto", "Nah", "Yalu", "Yuko", "Yamu","Mara", "Aare", "Aara", "Ness", "Yara"))
# Select a specific time period:
eloplot(KBELO,from="2021-09-30",to="2022-08-01")
# Specific IDs over a specific time period:
eloplot(KBELO, ids=c("Lif", "Yan", "Oke", "Tev", "Arn", "Eto", "Nah", "Yalu", "Yuko", "Yamu","Mara", "Aare", "Aara", "Ness", "Yara"),  from = "2020-05-09", to = "2021-12-17")

# Males
eloplot(KBELO, ids=c("Oke", "Tev", "Eto", "Aar", "Fur", "Pru"),  from = "2021-09-30",to="2022-08-01")

# Females
eloplot(KBELO, ids=c("Yamu", "Yara", "Yuko", "Aare", "Aara", "Mara"),  from = "2021-09-30",to="2022-08-01")

# Get scores
extract_elo(KBELO)

#to get elo score on a specific date
extract_elo(KBELO,"2020-12-18") 

#to get an average of the eloscore over a period 
extract_elo(KBELO, "2020-05-09", daterange=162) 

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(KBELO, "2020-05-09", daterange=162, standardize=TRUE) 


## KB Adult female dominance ####

KBaf <- subset(KB, KB$AggressorSex%in%("F"))
KBaf <- subset(KBaf, KBaf$VictimSex%in%("F"))
KBaf <- subset(KBaf, KBaf$AggressorAge > 3)
KBaf <- subset(KBaf, KBaf$VictimAge > 3)


## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=KBaf$winner, loser=KBaf$loser, Date=KBaf$Date, draw = NULL, presence= KBpres)
# Warnings:
# There is 1 case in which loser ID equals winner ID (check!)
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

KBafELO <- elo.seq(winner = KBaf$winner, loser=KBaf$loser, Date=KBaf$Date, presence = KBpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(KBafELO)
eloplot(KBafELO)
# Select specific IDs:
eloplot(KBafELO, ids=c("Aare", "Yuko", "Mara", "Yara", "Yamu", "Aara")) 
# Select a specific time period:
eloplot(KBafELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(KBafELO, ids=c("Aare", "Yuko", "Mara", "Yara", "Yamu", "Aara"), from="2021-09-30",to="2022-03-30")

extract_elo(KBafELO)

#to get elo score on a specific date
extract_elo(NHafELO,"2020-12-18")

#to get an average of the eloscore over a period -> here from 2020-05-09 to 2021-08-11
extract_elo(NHafELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHafELO, "2020-05-09", daterange=162, standardize=TRUE)


## KB Adult male dominance ####

KBam <- subset(KB, KB$AggressorSex%in%("M"))
KBam <- subset(KBam, KBam$VictimSex%in%("M"))
KBam <- subset(KBam, KBam$AggressorAge > 4)
KBam <- subset(KBam, KBam$VictimAge > 4)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=KBam$winner, loser=KBam$loser, Date=KBam$Date, draw = NULL, presence=KBpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

KBamELO <- elo.seq(winner = KBam$winner, loser=KBam$loser, Date=KBam$Date, presence = KBpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(KBamELO)
eloplot(KBamELO)
# Select specific IDs:
eloplot(KBamELO, ids=c("Lif", "Rey", "Wol", "Xia", "Yan", "Vul")) 
# Select a specific time period:
eloplot(KBamELO,from="2022-09-19", to = "2023-03-19")
# Specific IDs over a specific time period:
eloplot(NHamELO, ids=c("Lif", "Rey", "Wol", "Xia", "Yan", "Vul"), from="2020-01-01",to="2021-01-01")

extract_elo(KBamELO)

#to get elo score on a specific date
extract_elo(NHamELO,"2020-12-18")

#to get an average of the eloscore over a period 
extract_elo(NHamELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHamELO, "2020-05-09", daterange=162, standardize=TRUE)



#### Lemon Tree ELO-RATING ####

# To incorporate presence data
# First read the presence matrix

LTpres <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/LT2016-2022.csv", header=TRUE)
LTpres$Date  <- as.Date(as.character(LTpres$Date))
colnames(LTpres)[1] <- "Delete"
colnames(LTpres)[3] <- "Delete"
LTpres <- LTpres[,!grepl("Delete",names(LTpres))]

# Check if data look good

str(LTpres)
head(LTpres)
tail(LTpres)

## ALL LT INDIVIDUALS ####
## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=LT$winner, loser=LT$loser, Date=LT$Date, draw = NULL, presence=LTpres)
# Warnings:
# There are 3 cases in which loser ID equals winner ID
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (these ind apparantly didn't have any conflicts?)
LT <- LT[!(LT$winner == "Add" | LT$winner == "Nel" | LT$winner == "Potj"),]
LT <- LT[!(LT$loser == "Add" | LT$loser == "Nel" | LT$loser == "Potj"),]

LT <- LT[!(LT$Aggressor == "Rat" & LT$Date < "2022-06-17" | LT$Aggressor == "Bab" & LT$Date < "2022-06-17"),]
LT <- LT[!(LT$Victim == "Rat" & LT$Date < "2022-06-17" |  LT$Victim == "Bab" & LT$Date < "2022-06-17"),]


LTELO <- elo.seq(winner = LT$winner, loser=LT$loser, Date=LT$Date, presence = LTpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(LTELO)
eloplot(LTELO)

# Select specific IDs:
eloplot(NHELO, ids=c("Bos", "Gri", "Kek", "Lif", "Gan", "Fle", "Pro", "Xia", "Beir","Bela", "Gaya", "Gran", "Guat", "Upps", "Xian"))
# Select a specific time period:
eloplot(LTELO,from="2021-09-30",to="2022-08-01")
# Specific IDs over a specific time period:
eloplot(LTELO, ids=c("Rat", "Mom", "Apa", "Geo", "Daa", "Dian", "Digb", "Daen", "Deli", "Dext", "Dore", "Lill", "Loui", "Lizz", "Lanc", "Dewe", "Dais"),  from = "2021-09-30",to="2022-03-30")

# Males
eloplot(LTELO, ids=c("Rat", "Mom", "Apa", "Geo", "Daa"),  from = "2021-09-30",to="2022-08-01")

# Females 
eloplot(LTELO, ids=c("Dian", "Digb", "Daen", "Deli", "Dext", "Dore", "Lill", "Loui", "Lizz", "Lanc", "Dewe", "Dais"),  from = "2021-09-30",to="2022-03-30")

# Get scores
extract_elo(LTELO)

#to get elo score on a specific date
extract_elo(NHELO,"2022-03-30") 

#to get an average of the eloscore over a period 
extract_elo(NHELO, "2020-05-09", daterange=162) 

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHELO, "2020-05-09", daterange=162, standardize=TRUE) # Neu above Ouli


## LT Adult female dominance ####

LTaf <- subset(LT, LT$AggressorSex%in%("F"))
LTaf <- subset(LTaf, LTaf$VictimSex%in%("F"))
LTaf <- subset(LTaf, LTaf$AggressorAge > 3)
LTaf <- subset(LTaf, LTaf$VictimAge > 3)
# Select time period (optional)
#NHaf <- subset(NHaf, NHaf$Year.x > 2018)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=LTaf$winner, loser=LTaf$loser, Date=LTaf$Date, draw = NULL, presence= LTpres)
# Warnings:
# There is 1 case in which loser ID equals winner ID (check!)
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

LTafELO <- elo.seq(winner = LTaf$winner, loser=LTaf$loser, Date=LTaf$Date, presence = LTpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(LTafELO)
eloplot(LTafELO, from="2022-09-19", to = "2023-03-19")

extract_elo(LTafELO)


## LT Adult male dominance ####

LTam <- subset(LT, LT$AggressorSex%in%("M"))
LTam <- subset(LTam, LTam$VictimSex%in%("M"))
LTam <- subset(LTam, LTam$AggressorAge > 4)
LTam <- subset(LTam, LTam$VictimAge > 4)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=LTam$winner, loser=LTam$loser, Date=LTam$Date, draw = NULL, presence=LTpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

LTamELO <- elo.seq(winner = LTam$winner, loser=LTam$loser, Date=LTam$Date, presence = LTpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(LTamELO)
eloplot(LTamELO)

extract_elo(LTamELO)

#to get elo score on a specific date
extract_elo(NHamELO,"2020-12-18")

#to get an average of the eloscore over a period 
extract_elo(NHamELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHamELO, "2020-05-09", daterange=162, standardize=TRUE)



#### Crossing ELO-RATING ####

# To incorporate presence data
# First read the presence matrix

CRpres <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/CR2016-2022.csv", header=TRUE)
CRpres$Date  <- as.Date(as.character(CRpres$Date))
colnames(CRpres)[1] <- "Delete"
colnames(CRpres)[3] <- "Delete"
CRpres <- CRpres[,!grepl("Delete",names(CRpres))]

# Check if data look good

str(CRpres)
head(CRpres)
tail(CRpres)

## ALL CR INDIVIDUALS ####
## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=CR$winner, loser=CR$loser, Date=CR$Date, draw = NULL, presence=CRpres)
# Warnings:
# There are 3 cases in which loser ID equals winner ID
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (these ind apparantly didn't have any conflicts?)

# The following IDs occur in the data sequence but NOT in the presence data ... ERROR
# Ass
CR <- CR[!(CR$winner == "Ass" | CR$winner == "Heli"),]
CR <- CR[!(CR$loser == "Ass" | CR$loser == "Heli"),]

CR <- CR[!(CR$Aggressor == "Mvu" & CR$Date < "2016-12-29"),]
CR <- CR[!(CR$Victim == "Mvu" & CR$Date < "2016-12-29"),]

CRELO <- elo.seq(winner = CR$winner, loser=CR$loser, Date=CR$Date, presence = CRpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

# Can actually has double presence in NH but the presence table only includes the latest. Follow up on this

summary(CRELO)
eloplot(CRELO)

# Select specific IDs:
eloplot(CRELO, ids=c("Bos", "Gri", "Kek", "Lif", "Gan", "Fle", "Pro", "Xia", "Beir","Bela", "Gaya", "Gran", "Guat", "Upps", "Xian"))
# Select a specific time period:
eloplot(CRELO,from="2021-09-01",to="2022-03-01")
# Specific IDs over a specific time period:
eloplot(CRELO, ids=c("Bos", "Gri", "Kek", "Lif", "Gan", "Fle", "Pro", "Xia", "Beir","Bela", "Gaya", "Gran", "Guat", "Upps", "Xian"),  from = "2020-05-09", to = "2021-12-17")

# Males
eloplot(CRELO, ids=c("Xiu", "Hei", "Obe", "Cam"), from="2021-09-01",to="2022-03-01")

# Females
eloplot(CRELO, ids=c("Bali", "Chil", "Kodi", "Goos", "Gala", "Niha", "Mack"), from="2021-09-01",to="2022-03-01")

# Get scores
extract_elo(CRELO)

#to get elo score on a specific date
extract_elo(NHELO,"2020-12-18") 

#to get an average of the eloscore over a period 
extract_elo(NHELO, "2020-05-09", daterange=162) 

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHELO, "2020-05-09", daterange=162, standardize=TRUE) # Neu above Ouli


## NH Adult female dominance ####

NHaf <- subset(NH, NH$AggressorSex%in%("F"))
NHaf <- subset(NHaf, NHaf$VictimSex%in%("F"))
NHaf <- subset(NHaf, NHaf$AggressorAge > 3)
NHaf <- subset(NHaf, NHaf$VictimAge > 3)
# Select time period (optional)
#NHaf <- subset(NHaf, NHaf$Year.x > 2018)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=NHaf$winner, loser=NHaf$loser, Date=NHaf$Date, draw = NULL, presence= NHpres)
# Warnings:
# There is 1 case in which loser ID equals winner ID (check!)
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

NHafELO <- elo.seq(winner = NHaf$winner, loser=NHaf$loser, Date=NHaf$Date, presence = NHpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(NHafELO)
eloplot(NHafELO)
# Select specific IDs:
eloplot(NHafELO, ids=c("Bela", "Gaya", "Gran", "Guat", "Upps", "Xian", "Pret", "Prai", "Raba")) 
# Select a specific time period:
eloplot(NHafELO,from="2020-01-01",to="2021-01-01")
# Specific IDs over a specific time period:
eloplot(NHafELO, ids=c("Bela", "Gaya", "Gran", "Guat", "Upps", "Xian", "Pret", "Prai", "Raba"), from="2020-01-01",to="2021-01-01")

extract_elo(NHafELO)

#to get elo score on a specific date
extract_elo(NHafELO,"2020-12-18")

#to get an average of the eloscore over a period -> here from 2020-05-09 to 2021-08-11
extract_elo(NHafELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHafELO, "2020-05-09", daterange=162, standardize=TRUE)


## NH Adult male dominance ####

NHam <- subset(NH, NH$AggressorSex%in%("M"))
NHam <- subset(NHam, NHam$VictimSex%in%("M"))
NHam <- subset(NHam, NHam$AggressorAge > 4)
NHam <- subset(NHam, NHam$VictimAge > 4)

## WITH PRESENCE DATA 

# Check whether data looks good
seqcheck(winner=NHam$winner, loser=NHam$loser, Date=NHam$Date, draw = NULL, presence=NHpres)
# Warnings:
# Date column is not ordered (not huge problem)
# Presence starts earlier than data (makes sense)
# IDs in datasequence and presence do not match:
# The following IDs occur in the presence data but NOT in the data sequence ... (makes sense because we only selected adult females)

NHamELO <- elo.seq(winner = NHam$winner, loser=NHam$loser, Date=NHam$Date, presence = NHpres, runcheck=F)
# Set runcheck = F since the warnings seqcheck gives are not a dealbreaker

summary(NHamELO)
eloplot(NHamELO)
# Select specific IDs:
eloplot(NHamELO, ids=c("Lif", "Rey", "Wol", "Xia", "Yan", "Vul")) 
# Select a specific time period:
eloplot(NHamELO,from="2020-01-01",to="2021-01-01")
# Specific IDs over a specific time period:
eloplot(NHamELO, ids=c("Lif", "Rey", "Wol", "Xia", "Yan", "Vul"), from="2020-01-01",to="2021-01-01")

extract_elo(NHamELO)

#to get elo score on a specific date
extract_elo(NHamELO,"2020-12-18")

#to get an average of the eloscore over a period 
extract_elo(NHamELO, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(NHamELO, "2020-05-09", daterange=162, standardize=TRUE)









NHnow <- subset(NH, NH$AggressorSex%in%("F"))
NHnow <- subset(NHnow, NHnow$VictimSex%in%("F"))
NHnow <- subset(NHnow, NHnow$AggressorAge > 4)
NHnow <- subset(NHnow, NHnow$VictimAge > 4)
NHnow <- subset(NHnow, NHnow$Year.x > 2016)
NHnow <- NHnow[!(NHnow$Aggressor == "Rose" | NHnow$Aggressor == "Xala" | NHnow$Aggressor == "Xian"),]
NHnow <- NHnow[!(NHnow$Victim == "Rose" | NHnow$Victim == "Xala" | NHnow$Victim == "Xian"),]

seqcheck(winner=BDnow$winner, loser=BDnow$loser, Date=BDnow$Date, draw=NULL, presence=NULL)

res <- elo.seq(winner=BDnow$winner, loser=BDnow$loser, Date=BDnow$Date, runcheck=F)

res

BDnow$winner
BDnow$loser
BDnow$Date

summary(res)
extract_elo(res)
extract_elo(res,"2021-08-01")

eloplot(res)
eloplot(res,from="2021-08-01",to="2021-11-08")

# To incorporate presence data
# First read the presence matrix

xpres <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Presence/BD2016-2022.csv", header=TRUE)
xpres$Date  <- as.Date(as.character(xpres$Date))
xpres <- xpres[,!grepl("X",names(xpres))]

# Check if data look good

str(xpres)
head(xpres)
tail(xpres)

seqcheck(winner=BDnow$winner, loser=BDnow$loser, Date=BDnow$Date, draw = NULL, presence=xpres)
xdata$Actor
xdata$Recipient
xdata$Date
xpres

respres <- elo.seq(winner = BDnow$Aggressor, loser=BDnow$Victim, Date=BDnow$Date,presence = xpres, runcheck=F)

summary(respres)
eloplot(respres)
eloplot(res, ids=c("Che", "Ham", "Lek", "Neu", "Oku", "Tor", "Zur"))
eloplot(respres,from="2014-05-09",to="2014-10-17")
eloplot(respres, ids=c("Che", "Ham", "Lek", "Neu", "Oku", "Tor", "Zur", "Ouli", "Asis", "Mooi","Numb", "Riss", "Bemi", "Wiet" ), from = "2014-05-09", to = "2014-10-17")
extract_elo(respres)


#to get elo score on a specific date
extract_elo(respres,"2020-12-18")

#to get an average of the eloscore over a period -> here from 9.05.2014 until 17.10.2014 = 162 days later -> For the encounters taking focal data
extract_elo(respres, "2020-05-09", daterange=162)

#to standardize the ratings to scale them between 0 and 1 -> 1=top ranking one and 0 the last one
extract_elo(respres, "2020-05-09", daterange=162, standardize=TRUE)

range(BDnow$Date)
range(xpres$Date)
head(xpres)
tail(xpres)






##

################### FROM HERE ON IT IS VERY MESSY ######################
d$AggressorAge <- if(d$Aggressor == lh$Code){
  d$Year - lh$Year 
} else {
  NA
}

View(d)

seqcheck <- function(winner, loser, Date, draw = NULL, presence = NULL) {

  
  # Part 1:
  # check some general issues
  
  # creating checksum
  checksum <- rep(0, 15)
  names(checksum) <- c("IDcheck", "selfinteractions", "presence", "startpresence1", "startpresence2", "endpresence1", "endpresence2", "IDmatch", "IA_presencematch", "presenceentries", "datecol", "length", "singledayobs", "continouspres", "seqdateorder")
  
  Date <- as.Date(as.character(Date))
  # check whether dates for the interactions are in increasing order
  checkval <- min(as.numeric(diff(Date, unit = "day")))
  if (checkval < 0) checksum["seqdateorder"] <- 1
  
  # check whether all vectors contain same number of entries
  if (length(winner) != length(loser) | length(winner) != length(Date)) checksum["length"] <- 1
  
  
  # check draw/tie vector
  if (is.null(draw)) draw <- rep(FALSE, length(winner))
  if (length(winner) != length(draw)) checksum["length"] <- 1
  
  
  # the remaining checks are conditional on the fact that length of vectors match...
  
  if (checksum["length"] == 0) {
    
    datasequence <- data.frame(Winner = winner, Loser = loser, Date = Date)
    
    # check for integrity of IDs
    winners <- as.character(datasequence[, "Winner"])
    losers <- as.character(datasequence[, "Loser"])
    allIDs <- sort(unique(c(winners, losers)))
    if (length(allIDs) == length(unique(tolower(allIDs)))) {
      IDcheck <- "Everything seems alright with capitalization of IDs"
    } else {
      IDcheck <- "There seems to be a problem with capitalization of IDs?"
      checksum["IDcheck"] <- 1
    }
    
    # check whether IDs had interactions with themselves...
    selfinteractions <- paste("There is (are)",
                              length(which(winners == losers)),
                              "case(s) in which loser ID equals winner ID")
    if (length(which(winners == losers)) > 0) checksum["selfinteractions"] <- 1
    
    # check whether presence data is given
    if (!is.null(presence)) {
      presenceD <- "Presence data is supplied"
    } else {
      presenceD <- "Presence data is not supplied"
      checksum["presence"] <- 1
    }
    
    # check whether there is a column named Date in the presence matrix
    if (!is.null(presence)) {
      if ("Date" %in% colnames(presence)) {
        datecol <- "Date column found"
      } else {
        datecol <- "No 'Date' column found in supplied presence data"
        checksum["datecol"] <- 1
      }
    }
    
    # check whether there are gaps in the presence data...
    if (!is.null(presence) & checksum["datecol"] == 0) {
      if (nrow(presence) < as.numeric(diff(range(presence$Date))) + 1) {
        checksum["continouspres"] <- 1
        continouspres <- "There appear to be gaps in your presence data (missing days?)"
      }
    } else {
      continouspres <- "not checked"
    }
    
    
    
    # check whether date range in presence is the same as in sequence data
    START <- NA
    END <- NA
    if (!is.null(presence) & checksum["datecol"] == 0) {
      DATESpres <- as.Date(as.character(presence[, which(colnames(presence) %in% c("Date", "date"))]))
      DATESdata <- unique(as.Date(as.character(datasequence[, which(colnames(datasequence) %in% c("Date", "date"))])))
      if (min(DATESpres) < min(DATESdata)) {
        START <- "Presence starts earlier than data"
        checksum["startpresence1"] <- 1
      } # actually, not a problem per se
      if (min(DATESpres) > min(DATESdata)) {
        START <- "Presence starts AFTER data -> PROBLEM!"
        checksum["startpresence2"] <- 1
      }
      if (min(DATESpres) == min(DATESdata)) {
        START <- "Presence starts at the same date than data -> GOOD!"
      }
      if (max(DATESpres) < max(DATESdata)) {
        END <- "Presence stops BEFORE data -> PROBLEM!"
        checksum["endpresence1"] <- 1
      }
      if (max(DATESpres) > max(DATESdata)) {
        END <- "Presence continues beyond data"
        checksum["endpresence2"] <- 1
      } # actually, not a problem per se
      if (max(DATESpres) == max(DATESdata)) {
        END <- "Presence stops at the same date than data -> GOOD!"
      }
    }
    
    # check whether IDs match in data and presence
    if (!is.null(presence)) {
      IDdata <- sort(allIDs)
      IDpres <- sort(names(presence[, 2:ncol(presence)]))
      
      IDmatch <- "IDs in presence and data match -> GOOD!"
      
      # check whether
      if (length(which(!IDpres %in% IDdata)) > 0) {
        IDmatch1 <- IDpres[which(!IDpres %in% IDdata)]
      } else {
        IDmatch1 <- "none"
      }
      
      if (length(which(!IDdata %in% IDpres)) > 0) {
        IDmatch2 <- IDdata[which(!IDdata %in% IDpres)]
      } else {
        IDmatch2 <- "none"
      }
      
      if (IDmatch1[1] != "none" | IDmatch2[1] != "none") {
        IDmatch <- c(paste("The following IDs occur in the presence data but NOT in the data sequence:", IDmatch1), paste("the following IDs occur in the data sequence but NOT in the presence data:", IDmatch2))
        checksum[8] <- 1
      }
      
    }
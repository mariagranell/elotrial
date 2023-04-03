#### Check assumptions Elo package ####
# we are working

library(EloRating)
library(lubridate)
library(tidyverse)
source("/Users/mariagranell/Repositories/male_services_index/functions.R")

d <- read.csv("/Users/mariagranell/Repositories/master_students/josies_scripts/Agonistic.CSV", header=TRUE, na.strings=c(""," ","NA"))

d$Date <- dmy(d$Date)
df <- d
## Assumptions:
# File is ordered by date
# There are no empty cells
# IDs occur more than once
# Loser ID is different from winner ID

# Order by date
d <- dplyr::arrange(d, Date)

# TODO chekc the remarks
sum(!is.na(d$Remarks))

# TODO remove supportters rows. check later what if if you dont
d <- d %>% filter(is.na(IDSupporters))

# Omit NA's
d <-d %>% select(Date,Time,Group,Data,Context,IDIndividual1,BehaviourIndiv1,IDIndividual2,BehaviourIndiv2)
d[1884,5] <- "Unknown"
d <- na.omit(d)

#### Winner/loser ####
## Written by Stephanie Mercier ##

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

  # how many retret behaviour are in x and y?
  for(beh_ in ret_beh){
    x <- x + lengths(regmatches(beh_x, gregexpr(beh_, beh_x)))
    y <- y + lengths(regmatches(beh_y, gregexpr(beh_, beh_y)))
  }
  if(x < y) return(1)
  if(y < x) return(2)

  # how many aggresive behaviour are in x and y?
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


# TODO  for data cleaning
{# # write the function which allows to calculate for each sequence of obs between two individuals,
# # how many aggressive behaviours of the different severity each of them performed
# # (starting with the most intense one, i.e., involving physical contact), and as soon as one of the individuals
# # has a greater number of aggressive behaviours performed, we declare this individual as the winner
# # (ignoring the rest of the sequence, but making sure that this one is not the individual also ending up the conflict by moving away...)
# # compute.wins <- function(l.data, ind_x, ind_y){
# #
# #  fight.vec <- l.data$win_lose
# #
# #  return(c(sum(c(fight.vec[l.data$Aggressor==ind_x]==1,
# #                 fight.vec[l.data$Victim==ind_x]==2)),
# #           sum(c(fight.vec[l.data$Aggressor==ind_y]==1,
# #                 fight.vec[l.data$Victim==ind_y]==2))))
# #}

#  previously used to calculate dyadic rank -> not used anymore as looking at ISI's scores! but can use it to "clean" fight.data
# as Aggressors actually not always real aggressors!! So use the two previously written functions to decide for 
# each observation of our fight.data, which individual can be considered as the winner using all the conflicts 
# between these two individuals that occurred within the last year (365days):

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
}

seq.data <- fight.data

# define winner & loser using win_lose as 1 means that Aggressor is the most aggressive ind so the winner, 
# whereas 2 means that Victim is the most aggressive ind and thus the winner!
seq.data$winner <- NA
seq.data$BehaviourW <- NA
seq.data$loser <- NA
seq.data$BehaviourL <- NA

for(i in seq_len(nrow(seq.data))){
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

# add intensity: mild, chase, severe
seq.data$intensity <- NA
mild <- which(grepl("ap|ac|ag|at|dp|tp|st|vo|gu|sc|ap0|ap2|ap10|hb",seq.data$BehaviourW)=="TRUE")
seq.data[mild,"intensity"] <- "mild"
chase <- which(grepl("ch",seq.data$BehaviourW)=="TRUE")
seq.data[chase,"intensity"] <- "chase"
severe <- which(grepl("bi|gb|hi|fi|hh|so|fm",seq.data$BehaviourW)=="TRUE")
seq.data[severe,"intensity"] <- "severe"
# TODO some BehaviourW are not categorized by intensity. Check!

table(seq.data$intensity)
# Exclude the interactions that had no clear winners
X <- which(is.na(seq.data$intensity))
seq.data <- seq.data[-X,]

length(unique(seq.data$winner))
length(unique(seq.data$loser))

d <- seq.data
#d$Date <- as.Date(d$Date)
str(d)
d <- d %>% rename(WinnerBehaviour = BehaviourW, LoserBehaviour = BehaviourL)

## Link life history to individuals ####

setwd("/Users/mariagranell/Repositories/phllipe_vulloid/tbl_Creation/tbl_maria")

# select the tbls of interest: aniamlID, animalCode, age, sex, group, number of days in a group for males
tbl_AnimalID <- read.csv("tbl_AnimalID.csv")
tbl_AnimalCode <- read.csv("tbl_AnimalCode.csv")
tbl_Age <- read.csv("tbl_Age_notclean.csv")
tbl_Sex <- read.csv("tbl_Sex.csv")
tbl_OtherID <- read.csv("tbl_OtherID.csv")
tbl_DOB <- read.csv("tbl_DOB.csv")
tbl_GroupMembership <- read.csv("tbl_GroupMembership.csv")

# merge lh tables into one df
lh <- tbl_Sex %>%
  left_join(., tbl_Age %>%
              select(
                AnimalID, Age_yr, Age_class
              ), by = c("AnimalID" = "AnimalID")) %>%
  left_join(., tbl_OtherID, by = c("AnimalID" = "AnimalID"), multiple = "all") %>%
  left_join(., tbl_DOB, by = c("AnimalID" = "AnimalID"), multiple = "all") %>%
  left_join(., tbl_GroupMembership, by = c("AnimalID" = "AnimalID"), multiple = "all")
rm(tbl_Age,tbl_GroupMembership,tbl_Sex,tbl_AnimalCode,tbl_OtherID,tbl_AnimalID, tbl_DOB)
# remove rows that have no animal codes
lh <- lh[!is.na(lh$LH_AnimalCode),]





lh <- read.csv2("/Users/mariagranell/Repositories/data_summaries/life_history/IVP_Lifehistory_020822.csv", header = T)
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

## Create column with AggressorAge, AggressorSex, VictimAge, VictimSex

# To join dataframes you need a column with the same name and value

Agg <- lh %>% select(Sex, Year, Code) %>%
  rename(winner = Code)
Vic <- Agg %>% rename(loser = winner)

# join Aggressor dataframe with conflict dataframe
join <- dplyr::left_join(d, Agg, by = "winner", multiple = "all")

# calculate the age of the aggressor in the year of the interaction
join$AggressorAge <- year(join[, "Date"]) - join$Year

# rename column with sex of the aggressor
join <- join%>% rename(AggressorSex = Sex, YearAgressor = Year)

# join Victim dataframe with conflict dataframe
x <- dplyr::left_join(join, Vic, by = "loser", multiple = "all")

# calculate age of victim in year of interaction
x$VictimAge <- year(x[, "Date"]) - x$Year

# rename column with sex of the victim
x <- x%>% rename(VictimSex = Sex, YearVictim = Year)

d <-x

# Omit NA's ####
d <- na.omit(d)
# a bit of cleaning
d <- subset(d,winner != loser)

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

# TODO incorporate presence data from the logbook

#### ALL BD INDIVIDUALS ####
## WITH PRESENCE DATA

# Check whether data looks good
seqcheck(winner=BD$winner, loser=BD$loser, Date=BD$Date, draw = NULL)

# You need one year of data before the babies so. each grouo should have a different data. Take a month and a year before that date

BDELO <- elo.seq(winner = BD$winner, loser=BD$loser, Date=BD$Date, runcheck=F)
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

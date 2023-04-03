### Combining previous data ####
# PenDragon Data #
## ELO-RATING ##
# DAY - MONTH - YEAR - TIME - ACTOR - AGGRESSOR BEHAVIOUR - RECIPIENT - VICTIM BEHAVIOUR

elo <- read.csv("/Users/mariagranell/Repositories/master_students/josies_scripts/AgonisticJunOct.csv",1)

# Delete second row with double headers

elo <- elo[!(elo$RecordId == "RecordId"),]
#View(elo)

# We only use one on one interactions
# So omit all interactions with a supporter

elo <- dplyr::filter(elo, Supporter == "")

# Select columns of interest

elo <- elo[,c(5,6,8:11)]
#View(elo)

# Split up Day, Month, Year and Time

elo$Time <- format(as.POSIXct(elo$DateTime,format="%m/%d/%Y %H:%M:%S"),"%H:%M:%S")
elo$Day <- format(as.POSIXct(elo$DateTime, format = "%m/%d/%Y %H:%M:%S"), "%d")
elo$Month <- format(as.POSIXct(elo$DateTime, format = "%m/%d/%Y %H:%M:%S"), "%m")
elo$Year <- format(as.POSIXct(elo$DateTime, format = "%m/%d/%Y %H:%M:%S"), "%Y")

elo$Day <- as.integer(elo$Day)
elo$Month <- as.integer(elo$Month)
elo$Year <- as.integer(elo$Year)
elo$Time <- as.factor(elo$Time)
str(elo)

# Select columns of interest

elo <- elo[,c(8:10,7,2:6)]

# Create csv file 
write.csv(elo, "C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/Conflict090522.csv")

## Cybertracker Data ##
## ELO-RATING ##
# DAY - MONTH - YEAR - TIME - ACTOR - AGGRESSOR BEHAVIOUR - RECIPIENT - VICTIM BEHAVIOUR

elo <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/AgonisticAprMay22.csv")

# We only use one on one interactions
# So omit all interactions with a supporter

elo <- dplyr::filter(elo, Supporters == "")

# Select columns of interest

elo <- elo[,c(1:3,7:10)]

str(elo)

# Put in right format

elo$Time <- format(as.POSIXct(elo$Time,format="%H:%M:%S"),"%H:%M:%S")
colnames(elo)[1] <- "Date"
elo$Date <- as.Date(format(as.POSIXct(elo$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
str(elo)

# Merge with PenDragon data

PD <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/Conflict090522.csv",1)

str(PD)

# Create a column that R recognizes as Date
PD$Date <- as.Date(strptime(paste(PD$Year, PD$Month, PD$Day), format="%Y %m %d"))

PD$X <- PD$Date
colnames(PD)[1] <- "Date"

# Select columns of interest

PD <- PD[,c(1,5:10)]

colnames(elo)[4] <- "Aggressor"
colnames(elo)[5] <- "AggressorBehaviour"
colnames(elo)[6] <- "Victim"
colnames(elo)[7] <- "VictimBehaviour"

old <- dplyr::bind_rows(elo, PD)

# Create csv file 
write.csv(old, "C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/PDandMayCombinedElo.csv")

# Ad lib #### NEWEST FILE ####
elo <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/Agonistic.csv",1)
#View(elo)

# We only use one on one interactions
# So omit all interactions with a supporter

elo <- dplyr::filter(elo, IDSupporters == "")

# Select columns of interest

elo <- elo[,c(1:3,7:10)]

str(elo)

# Put in right format

elo$Time <- format(as.POSIXct(elo$Time,format="%H:%M:%S"),"%H:%M:%S")
colnames(elo)[1] <- "Date"
elo$Date <- as.Date(format(as.POSIXct(elo$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
colnames(elo)[4] <- "Aggressor"
colnames(elo)[5] <- "AggressorBehaviour"
colnames(elo)[6] <- "Victim"
colnames(elo)[7] <- "VictimBehaviour"
str(elo)


# Merge with old combined data

old <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/AgonisticJunOct.csv",1)

# We only use one on one interactions
# So omit all interactions with a supporter

old <- dplyr::filter(old, IDSupporters == "")

# Select columns of interest
old <- old[,c(1:3,7:10)]

# Put in right format
old$Time <- format(as.POSIXct(old$Time,format="%H:%M:%S"),"%H:%M:%S")
old$Date <- as.Date(format(as.POSIXct(old$Date, format = "%d/%m/%Y"), "%Y-%m-%d"))
str(old)

colnames(old)[4] <- "Aggressor"
colnames(old)[5] <- "AggressorBehaviour"
colnames(old)[6] <- "Victim"
colnames(old)[7] <- "VictimBehaviour"

# Combine
new <- dplyr::bind_rows(elo, old)

# Combine with PD and May22 data
pd <- read.csv("C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/PDandMayCombinedElo.csv")
pd <- pd[,c(2:8)]
pd$Date <- as.Date(format(as.POSIXct(pd$Date, format = "%Y-%m-%d"), "%Y-%m-%d"))

new <- dplyr::bind_rows(new, pd)

## Aggressor not yet aggressor and victim not yet victim
write.csv(new, "C:/Users/josef/OneDrive - unine.ch/Documents/PhD/IVP DATA/Dominance hierarchy/PD-March23.csv")

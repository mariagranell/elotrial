# add crossing numbers ####
# Loop does not work if there are any NA's in the dataframe
c <- df
obs.nr <- 1
for(row.nr in 1:length(c$Date)) {
  if(row.nr == 1) {
    c$Obs.nr[row.nr] <- 1
  } else {
    time1 <- as.numeric(as.POSIXct(c$Time[row.nr], format = "%H:%M:%S")) +
      as.numeric(as.POSIXct(c$Date[row.nr], format = "%Y-%m-%d"))
    time0 <- as.numeric(as.POSIXct(c$Time[row.nr - 1], format = 
"%H:%M:%S")) +
      as.numeric(as.POSIXct(c$Date[row.nr - 1], format = "%Y-%m-%d"))
    timeinterval <- time1 - time0
    if(c$Group[row.nr] != c$Group[row.nr - 1] | (timeinterval > 900) | 
c$Date[row.nr] != c$Date[row.nr - 1]) {
      obs.nr <- obs.nr + 1
    }
    c$Obs.nr[row.nr] <- obs.nr
  }
}
View(c)

c <- d

meanNA <- function(x, ...) if (all(is.na(x))) NA else mean(x, ...)

c <- transform(c, GPSE = na.aggregate(GPSE, by = Obs.nr, FUN = meanNA))
c <- transform(c, GPSS = na.aggregate(GPSS, by = Obs.nr, FUN = meanNA))
c <- c %>% group_by(Obs.nr) %>% mutate(CrossingType = CrossingType[!is.na(CrossingType) ][1])
c <- c %>% group_by(Obs.nr) %>% mutate(Context = Context[ !is.na(Context)][1])
c <- c %>% group_by(Obs.nr) %>% mutate(Observers = Observers[!is.na(Observers) ][1])
c <- c %>% group_by(Obs.nr) %>% mutate(Remarks = Remarks[ !is.na(Remarks)][1])
c <- c %>% group_by(Obs.nr) %>% mutate(DataInfo = DataInfo[!is.na(DataInfo) ][1])

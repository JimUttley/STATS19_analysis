### SCRIPT FOR ASSESSING AFTER-DARK RISK FOR YOUNG (NEW) DRIVERS THROUGH STATS19 ANALYSIS ###

library(plyr)
library(stats19)
library(data.table)
library(dplyr)
library(ggplot2)
library(reshape2)
library(suncalc)
library(fmsb)

rootdir <- "D:/Google Drive/Research/STATS19"
# rootdir <- "C:/Users/Jim/Documents/Work stuff/STATS19"
setwd(rootdir)


dataVeh <- list()
dataAcc <- list()
n <- 0
startYear <- 2015
endYear <- 2017
ageThreshold <- 20

# Loop for downloading vehicle and accident stats19 data between startYear and endYear
for(i in seq(startYear,endYear,by=1)){
  n <- n+1
  dfVeh <- get_stats19(year=i,type="Vehicles",ask=F)
  dataVeh[[n]] <- dfVeh
  dfAcc <- get_stats19(year=i,type="Accidents",ask=F)
  dataAcc[[n]] <- dfAcc
}

# Combine vehicle and accident data into one data frame
dfVeh <- rbindlist(dataVeh,use.names=T,fill=T)
dfAcc <- rbindlist(dataAcc,use.names=T,fill=T)
dfAll <- left_join(dfVeh,dfAcc)

# Add additional columns to data frame about light conditions, age and time of RTc
dfAll$Light <- ifelse(dfAll$light_conditions=="Daylight","Day",ifelse(dfAll$light_conditions=="Data missing or out of range",NA,"Dark"))
dfAll$Age <- ifelse(dfAll$age_of_driver<ageThreshold,"Young","Older")
dfAll$Time <- as.numeric(substr(dfAll$time,1,2))

# Add details about time of sunset on date and at location of each RTC, using suncalc package
sunsetDF <- data.frame(date=as.Date(dfAll$date,tz="Europe/London"),lat=dfAll$latitude,lon=dfAll$longitude,tz="Europe/London")
dfAll$Sunset <- getSunlightTimes(data=sunsetDF,tz="Europe/London")$sunset


### Creating plot for RTCs per 1000 licence holders by age group of driver ###
licencesDF <- read.csv("Licence_holders_by_age_2017.csv") # Data from DfT Table DRL0101, "Provisional and full driving licences held, by age and by gender...."
licences <- merge(as.data.frame(table(as.numeric(dfAll$age_of_driver),dnn=list("Age")),responseName="RTCs"),licencesDF,by=c("Age"))
licences$Age <- as.numeric(licences$Age)
licences$ageBin <- cut(licences$Age,seq(16,105,by=3))
licences <- ddply(licences,"ageBin",summarise,sumRTC=sum(RTCs),sumLicence=sum(LicenceHolders),rate=(sumRTC/n)/(sumLicence/1000))

licencesPlot <- ggplot(data=subset(licences,!is.na(ageBin)),aes(x=ageBin,y=rate,group=1))+
  geom_line(size=1.5)+
  theme_classic()+
  labs(x="\nAge group of driver",y="Annual RTCs per 1000 licence holders\n")+
  scale_x_discrete(labels=c(paste(seq(17,101,by=3),seq(19,103,by=3),sep="-")))+
  theme(axis.title = element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90,size=15),
        axis.text.y = element_text(size=15))







### Plot showing temporal distribution of RTCs by age group ###
timeTab <- as.data.frame.matrix(table(dfAll$Time,dfAll$Age))
timeTab$Older <- timeTab$Older/max(timeTab$Older)
timeTab$Young <- timeTab$Young/max(timeTab$Young)
timeTab$Time <- as.numeric(rownames(timeTab))
timeTab <- melt(timeTab,id.vars="Time",value.name="propMax",variable.name="DriverAge")

timePlot <- ggplot(timeTab,aes(x=Time,y=propMax,group=DriverAge,colour=DriverAge))+
  theme_classic()+
  labs(x="\nTime of day",y="Hourly count of RTCs as\nproportion of max hourly count\n")+
  geom_line(size=1.5)+
  scale_x_continuous(breaks=c(0,4,8,12,16,20,24))+
  scale_colour_discrete(name="Age of driver",labels=c(paste(ageThreshold,"+",sep=""),paste("Under ",ageThreshold,sep="")),guide = guide_legend(reverse=TRUE))+
  theme(axis.title = element_text(size=20,face="bold"),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15),
        legend.position = "top",
        legend.title = element_text(size=15,face="bold"),
        legend.text = element_text(size=15))

### RTCs by light condition and age of driver ###
lightAge <- prop.table(table(dfAll$Light,dfAll$Age),margin=2)


### ODDS RATIO ANALYSIS ###
controlHour <- 14
caseHour <- 18
dfCC <- subset(dfAll,Time %in% c(controlHour,caseHour))
dfCC$CaseControl <- ifelse(dfCC$Time==14,"Control","Case")
dfCC$DayDark <- ifelse(dfCC$CaseControl=="Case",ifelse(as.POSIXct(paste(dfCC$date,dfCC$time),format="%Y-%m-%d %H:%M")<dfCC$Sunset,"Day","Dark"),
                       ifelse(dfCC$Sunset<as.POSIXct(paste(dfCC$date,"18:30:00"),format="%Y-%m-%d %H:%M"),"Dark","Day"))

# Odds ratio without using control hour, directly comparing young vs older
youngDark <- length(dfCC[dfCC$CaseControl=="Case"&dfCC$Age=="Young"&dfCC$DayDark=="Dark",]$DayDark)
youngDay <- length(dfCC[dfCC$CaseControl=="Case"&dfCC$Age=="Young"&dfCC$DayDark=="Day",]$accident_index)
oldDark <- length(dfCC[dfCC$CaseControl=="Case"&dfCC$Age=="Older"&dfCC$DayDark=="Dark",]$accident_index)
oldDay <- length(dfCC[dfCC$CaseControl=="Case"&dfCC$Age=="Older"&dfCC$DayDark=="Day",]$accident_index)
ORYoungOld <- oddsratio(youngDark,youngDay,oldDark,oldDay)

# Odds ratio using control hours - after-dark risk overall, and for young and older drivers
youngDarkControl <- length(dfCC[dfCC$CaseControl=="Control"&dfCC$Age=="Young"&dfCC$DayDark=="Dark",]$accident_index)
youngDayControl <- length(dfCC[dfCC$CaseControl=="Control"&dfCC$Age=="Young"&dfCC$DayDark=="Day",]$accident_index)
oldDarkControl <- length(dfCC[dfCC$CaseControl=="Control"&dfCC$Age=="Older"&dfCC$DayDark=="Dark",]$accident_index)
oldDayControl <- length(dfCC[dfCC$CaseControl=="Control"&dfCC$Age=="Older"&dfCC$DayDark=="Day",]$accident_index)

ORoverall <- oddsratio(youngDark+oldDark,youngDay+oldDay,youngDarkControl+oldDarkControl,youngDayControl+oldDayControl)
ORyoung <- oddsratio(youngDark,youngDay,youngDarkControl,youngDayControl)
ORold <- oddsratio(oldDark,oldDay,oldDarkControl,oldDayControl)


### Plot showing new licences by age ###
licencesDF$NewLicencesCP <- cumsum(licencesDF$NewLicences2018_19)/sum(licencesDF[!is.na(licencesDF$NewLicences2018_19),]$NewLicences2018_19)

licenceAgePlot <- ggplot(data=subset(licencesDF,!is.na(NewLicencesCP)),aes(x=Age,y=NewLicencesCP,group=1))+
  geom_line(size=1.5)+
  theme_classic()+
  ylim(0,1)+
  labs(x="\nAge ",y="Cumulative proportion of\nnew licences, 2017-18\n")+
  theme(axis.title = element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90,size=15),
        axis.text.y = element_text(size=15))





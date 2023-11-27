library(dplyr)
Data <- read.csv("Documents/Documents/SFU/ASNA/ASNA 2024 Case Competition Database.csv", header = TRUE)
Data
#step1: examine each variable as a histogram
#each hour per day
#windspeed
library(ggplot2)
windspeed <- Data$Wind.Speed..m.s.
year <- Data$Year
month <- Data$Month
day <- Data$Day
hour <- Data$Hour
windspeedex1 <- data.frame(year, month, day, hour, windspeed)
windspeedex1_subset <- subset(windspeedex1, year == "1998"& month == "1" & day == "1")
wex1_subsetplot<- ggplot(windspeedex1_subset, aes(hour, windspeed))+
  geom_point(color="green", size=3)
wex1_subsetplot
#solar radiation
solarghi <- Data$Clearsky.GHI..w.m2.
solarghiex1 <- data.frame(year, month, day, hour, solarghi)
solarghiex1_subset <- subset(solarghiex1, year == "1998" & month == "1" & day == "1")
ghiex1_subsetplot <- ggplot(solarghiex1_subset, aes(hour, solarghi))+
  geom_point(color="yellow", size=3)
ghiex1_subsetplot
#dhi
solardhi <- Data$Clearsky.DHI..w.m2.
solardhiex1 <- data.frame(year, month, day, hour, solardhi)
solardhiex1_subset <- subset(solardhiex1, year == "1998" & month == "1" & day == "1")
dhiex1_subsetplot <- ggplot(solardhiex1_subset, aes(hour, solardhi))+
  geom_point(color="orange", size=3)
dhiex1_subsetplot
#temperature
temp <- Data$Temperature..Celsius.
tempex1 <- data.frame(year, month, day, hour, temp)
tempex1_subset <- subset(tempex1, year == "1998" & month =="1" & day == "1")
tempex1_subsetplot <- ggplot(tempex1_subset, aes(hour, temp))+
  geom_point(color="blue", size=3)
tempex1_subsetplot

#each day per month
#windspeed
library(tidyverse)
library(lubridate)
#form a date variable
date <- paste(year, month, day, sep = "-", collapse=NULL)
date_hour <- paste(date, hour, sep = " ", collapse=NULL)
windspeedex2 <- data.frame(date_hour, windspeed)
windspeedex2_1 <- data.frame(datetime = date_hour, windspeed = windspeed)
wex2_1_arranged <- windspeedex2_1 %>% group_by(date=date(datetime)) %>%
  summarize(windspeed=mean(windspeed))
wex2_1_arranged
wex2_1plot <- ggplot(wex2_1_arranged, aes(date, windspeed))+
  geom_point(color="green", size = 0.01)+
  geom_smooth(method=lm, level=0.99)+
  xlab("Year")+
  ylab("Daily Avg Windspeed (m/s)")
wex2_1plot

#solar ghi
ghiex2 <- data.frame(date_hour, solarghi)
ghiex2_1 <- data.frame(datetime = date_hour, solarghi = solarghi)
ghiex2_1_arranged <- ghiex2_1 %>% group_by(date=date(datetime)) %>%
  summarize(solarghi=mean(solarghi))
ghiex2_1_arranged
ghiex2_1plot <- ggplot(ghiex2_1_arranged, aes(date, solarghi))+
  geom_point(color="yellow", size = 0.01)+
  geom_smooth(method=lm, level=0.99)+
  xlab("Year")+
  ylab("Daily Avg GHI (W/m^2)")
ghiex2_1plot

#temperature
tempex2 <- data.frame(date_hour, temp)
tempex2_1 <- data.frame(datetime = date_hour, temp=temp)
tempex2_1_arranged <- tempex2_1 %>% group_by(date=date(datetime)) %>%
  summarize(temp=mean(temp))
tempex2_1_arranged
tempex2_1plot <- ggplot(tempex2_1_arranged, aes(date, temp))+
  geom_point(color="blue", size = 0.01)+
  geom_smooth(method=lm, level=0.99)+
  xlab("Year")+
  ylab("Daily Avg Temperature (Celsius)")
tempex2_1plot

#monthly averages through 22 years
library(purrr)
tempmonthly <- tempex2_1_arranged %>%
  group_by(lubridate::month(date),lubridate::year(date) ) %>%
  summarise(temp = mean(temp))
tempmonthly
tempex2_1_arranged$date <- as.Date(tempex2_1_arranged$date)
tempex2_1_arranged$month <- months(tempex2_1_arranged$date)
tempex2_1_arranged$year <- format(tempex2_1_arranged$date, format="%y")
tempmonthly <- aggregate(temp ~ year + month, tempex2_1_arranged, mean)
tempmonthly
colnames(tempmonthly)[colnames(tempmonthly)=="lubridate::month(date)"] <- "Month"
colnames(tempmonthly)[colnames(tempmonthly)=="lubridate::year(date)"] <- "Year"


windmonthly1 <- wex2_1_arranged %>%
  group_by(lubridate::month(date),lubridate::year(date) ) %>%
  summarise(windspeed = mean(windspeed))
windmonthly1
wex2_1_arranged$date <- as.Date(wex2_1_arranged$date)
wex2_1_arranged$month <- months(wex2_1_arranged$date)
wex2_1_arranged$year <- format(wex2_1_arranged$date, format="%y")
windmonthly <- aggregate(windspeed ~ year + month, wex2_1_arranged, mean)
windmonthly
colnames(windmonthly)[colnames(windmonthly)=="lubridate::month(date)"] <- "Month"
colnames(windmonthly)[colnames(windmonthly)=="lubridate::year(date)"] <- "Year"

ghimonthly1 <- ghiex2_1_arranged %>%
  group_by(lubridate::month(date),lubridate::year(date) ) %>%
  summarise(solarghi = mean(solarghi))
ghimonthly1
ghiex2_1_arranged$date <- as.Date(ghiex2_1_arranged$date)
ghiex2_1_arranged$month <- months(ghiex2_1_arranged$date)
ghiex2_1_arranged$year <- format(ghiex2_1_arranged$date, format="%y")
ghimonthly <- aggregate(solarghi ~ year + month, ghiex2_1_arranged, mean)
ghimonthly
colnames(ghimonthly)[colnames(ghimonthly)=="lubridate::month(date)"] <- "Month"
colnames(ghimonthly)[colnames(ghimonthly)=="lubridate::year(date)"] <- "Year"

#convert monthly to Excel
write.csv(tempmonthly,file="Documents/Documents/SFU/ASNA/temp.csv")
write.csv(windmonthly,file="Documents/Documents/SFU/ASNA/windspeed.csv")
write.csv(ghimonthly,file="Documents/Documents/SFU/ASNA/ghi.csv")

#FINISHED
library(readxl)
tippi <- read_excel("Documents/Documents/SFU/ASNA/DHI, wind and temp.xlsx")
View(tippi)   
library(lubridate)
wind <- tippi$windspeed
year <- tippi$year
month <- tippi$month
n.month <- match(month, month.name)
ym <- paste(year, n.month, sep = "-")
historicwind <- data.frame(ym, wind)
historicwind
#plot historic monthly time series of windspeed
historicwind$ym <- as.Date(paste(historicwind$ym, "-01", sep=""))
historicwind$ym <- ymd(historicwind$ym)
class(historicwind$ym)
summary(historicwind$ym)
historicwindplot <- ggplot(historicwind, aes(ym, wind))+
  geom_point(color="green", size = 0.01)+
  geom_smooth(method=lm, level=0.99)+
  xlab("Year")+
  scale_x_date(date_breaks="5 years", date_minor_breaks = "6 months", date_labels="%Y")+
  ylab("Monthly Avg Windspeed (m/s)")
historicwindplot
#ghi
ghi <- tippi$solarghi
year <- tippi$year
month <- tippi$month
n.month <- match(month, month.name)
ym <- paste(year, n.month, sep = "-")
historicghi <- data.frame(ym, ghi)
historicghi
#plot historic monthly time series of windspeed
historicghi$ym <- as.Date(paste(historicghi$ym, "-01", sep=""))
historicghi$ym <- ymd(historicghi$ym)
class(historicghi$ym)
summary(historicghi$ym)
historicghiplot <- ggplot(historicghi, aes(ym, ghi))+
  geom_point(color="yellow", size = 1)+
  geom_smooth(method=lm, level=0.99)+
  xlab("Year")+
  scale_x_date(date_breaks="5 years", date_minor_breaks = "6 months", date_labels="%Y")+
  ylab("Monthly Avg GHI (W/m^2)")
historicghiplot
#temp
temp <- tippi$temp
year <- tippi$year
month <- tippi$month
n.month <- match(month, month.name)
ym <- paste(year, n.month, sep = "-")
historictemp <- data.frame(ym, temp)
historictemp
#plot historic monthly time series of windspeed
historictemp$ym <- as.Date(paste(historictemp$ym, "-01", sep=""))
historictemp$ym <- ymd(historictemp$ym)
class(historictemp$ym)
summary(historictemp$ym)
historictempplot <- ggplot(historictemp, aes(ym, temp))+
  geom_point(color="blue", size = 1)+
  geom_smooth(method=lm, level=0.99)+
  xlab("Year")+
  scale_x_date(date_breaks="5 years", date_minor_breaks = "6 months", date_labels="%Y")+
  ylab("Monthly Avg Temp (Celsius)")
historictempplot
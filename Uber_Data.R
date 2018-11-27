#initializing all the libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(chron)

#loading the uber data in Uber_Data
Uber_Data<-read.csv("Uber Request Data.csv",stringsAsFactors = F)

# looking at data
str(Uber_Data)
head(Uber_Data)

#checking for duplicate Request id
Uber_Data[which(duplicated(Uber_Data$Request.id)),]


##The date and time are in an inconsistent format, using lubridate library to parse into common R format

Uber_Data$Request.timestamp <- parse_date_time(Uber_Data$Request.timestamp,
                                                 orders = c("%d-%m-%y %H:%M:%S","%d-%m-%y %H:%M"),
                                                 locale = "eng")
Uber_Data$Drop.timestamp <- parse_date_time(Uber_Data$Drop.timestamp,
                                              orders=c("%d-%m-%y %H:%M:%S","%d-%m-%y %H:%M"),
                                              locale = "eng")


#deriving Hours and date from the Request timestamp Column
Hours <- format(as.POSIXct(strptime(Uber_Data$Request.timestamp,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
date <- (as.Date(Uber_Data$Request.timestamp ))


#and adding both Hours and date column to Data frame
Uber_Data<-cbind(Uber_Data,date)
Uber_Data<-cbind(Uber_Data,Hours)

#converting the Hours column to R time format for dividing the day into segments
Uber_Data$Hours<-chron(times=Uber_Data$Hours)

#sorting the data frame in ascending order of Request Id
Uber_Data<- Uber_Data[order(Uber_Data$Request.timestamp),]


#dividing the day into different Segments based on pickup time
Uber_Data$Segment[Uber_Data$Hours>=chron(times="00:00:00") & Uber_Data$Hours<chron(times="06:00:00")]<-"Early morning"
Uber_Data$Segment[Uber_Data$Hours>=chron(times="06:00:00") & Uber_Data$Hours<chron(times="12:00:00")]<-"Morning"
Uber_Data$Segment[Uber_Data$Hours>=chron(times="12:00:00") & Uber_Data$Hours<chron(times="16:00:00")]<-"Afternoon"
Uber_Data$Segment[Uber_Data$Hours>=chron(times="16:00:00") & Uber_Data$Hours<chron(times="20:00:00")]<-"Evening"
Uber_Data$Segment[Uber_Data$Hours>=chron(times="20:00:00") & Uber_Data$Hours<chron(times="23:59:59")]<-"Night"


#checking for NA values after the conversion
Uber_Data[which(is.na(Uber_Data$Segment)),]
Uber_Data[which(is.na(Uber_Data$Hours)),]

#converting Hours into absolute hours 
Uber_Data$Hours <- format(as.POSIXct(strptime(Uber_Data$Hours,"%H:%M:%S",tz="")) ,format = "%H")

#plotting the frequency of cancelled, trip completed and No cars available by date and pickup point
ggplot(Uber_Data,aes(x=factor(Status)))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)+facet_wrap(~date)
ggplot(Uber_Data,aes(x=factor(Pickup.point),fill=factor(Status)))+geom_bar()+facet_wrap(~date)

#plotting the number of trips with different statuses by segment and hour for both pickup points
ggplot(Uber_Data,aes(x=factor(Status),fill=factor(Segment)))+geom_bar()+facet_wrap(~Pickup.point)
ggplot(Uber_Data,aes(x=factor(Hours),fill=factor(Status)))+geom_bar()+facet_wrap(~Pickup.point)

#plotting the total nummber of requests by Status for all time segments to do demand supply analysis. 
ggplot(Uber_Data,aes(x=factor(Status),fill=factor(Pickup.point)))+geom_bar()+facet_wrap(~Segment)

#plotting the requests by Status by Hours and segment for both pickup points
ggplot(Uber_Data,aes(x=factor(Status),fill=factor(Hours)))+geom_bar() + facet_wrap(~Pickup.point)
ggplot(Uber_Data,aes(x=factor(Segment),fill=factor(Status)))+geom_bar() +facet_wrap(~Pickup.point)

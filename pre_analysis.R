
# Premilinary analysis
# setwd('DataScience/GIT/RepData_PeerAssessment1')
# Load data
data<-read.csv('activity.csv')
# Preprocessing data
library(dplyr)
data<-mutate(data,date=as.POSIXct(date,format='%Y-%m-%d'))

## First question
dataxday<-group_by(data,date)
dataxday<-summarize(dataxday,total=sum(steps))

# Total steps per day
# Histogram
hist(dataxday$total)
# mean na.rm=TRUE
avgd<-mean(dataxday$total,na.rm=TRUE)
# median
medd<-median(dataxday$total,na.rm=TRUE)
totd<-sum(dataxday$total,na.rm=TRUE)
print("Mean of original dataset is:"); print(avgd)
print("Median of original dataset is:");print(medd)


## Second question
stepxint<-group_by(data,interval)
stepxint<-summarize(stepxint,avgstep=mean(steps))

#plot
plot(stepxint$interval,stepxint$avgstep,type='l')
##
## p <- ggplot(...) + ...
## p + scale_x_discrete(breaks=seq(0, 2400, by=100))
##
## Assuming the data, going to x axis, is formatted as POSIXct
## scale_x_datetime(breaks = date_breaks("1 hour"), labels = date_format("%H:%M"))
#
# which 5 minutes interval with max average number per day
int<-stepxint$interval[stepxint$avgstep==max(stepxint$avgstep)]
h<-as.integer(int/100)
m<-int%%100
print(paste(h,m,sep=":"))


## Third question
#
# total number of NAs in the dataset
sum(is.na(data$date))
sum(is.na(data$interval))
sum(is.na(data$steps))

newdata<-data
iddna<-which(is.na(newdata$steps))
n<-length(iddna)

FUNFIL<-function(int){round(stepxint$avgstep[stepxint$interval==int])}

for (i in 1:n) {
idint<-iddna[i]
newdata$steps[idint]<-FUNFIL(newdata$interval[idint])
}

## Fourth question
ndataxday<-group_by(newdata,date)
ndataxday<-summarize(ndataxday,total=sum(steps))

# Total steps per day
# Histogram
hist(ndataxday$total)
# mean na.rm=TRUE
avgnd<-mean(ndataxday$total,na.rm=TRUE)
# median
mednd<-median(ndataxday$total,na.rm=TRUE)

totnd<-sum(ndataxday$total,na.rm=TRUE)
print("Mean of new dataset is:"); print(avgnd)
print("Median of new dataset is:");print(mednd)

print("Mean delta is"); print(avgnd-avgd)
print("Median delta is"); print(mednd-medd)
print("Total amount of input missing steps is"); print(totnd-totd)

## FIFTH question
## managing datetime in the plot
Sys.setlocale("LC_TIME", "English") ## sorry my personal LOCALE is Italian
newdata<-mutate(newdata,wk=factor(weekdays(date) %in% c('Sunday','Saturday'), labels = c("weekday", "weekend")))

stepxwint<-group_by(newdata,wk,interval)
stepxwint<-summarize(stepxwint,avgstep=mean(steps))

library(ggplot2)
#g<-qplot(interval,avgstep,data=stepxwint,facets=wk~.,main="Title")
#g<-g+geom_line()
#print g
qplot(interval,avgstep,data=stepxwint,geom='path',facets=wk~.,main="Title")


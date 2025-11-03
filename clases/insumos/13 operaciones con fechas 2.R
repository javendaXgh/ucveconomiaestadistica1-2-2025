library(quantmod)
library(lubridate)
library(dplyr)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
sampleTimes
dim(sampleTimes)
class(sampleTimes)
procesada1<-as.data.frame(sampleTimes)
dim(procesada1)
procesada1<-sampleTimes
procesada5<-as.data.frame(procesada1)
procesada2<-ymd(sampleTimes)
procesada2
procesada3<-wday(procesada2, label=T)
procesada3
procesada4<-year(sampleTimes)
head(procesada4)
procesada5=mutate(procesada5, procesada3=procesada3, procesada4=procesada4)
head(procesada5)

procesada6<- filter(procesada5, procesada4==2012)
dim(procesada6)
procesada7<- filter(procesada6, procesada3=="Mon")
dim(procesada7)
procesada6
procesada7

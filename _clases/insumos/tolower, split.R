rm(list = ls())

url_w4_1<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url_w4_1,"week4_1.csv")
week4_1<-read.csv("week4_1.csv")
names(week4_1)
names(week4_1)<-tolower(names(week4_1))
splitNames=strsplit(names(week4_1),"wgtp")
splitNames[123]

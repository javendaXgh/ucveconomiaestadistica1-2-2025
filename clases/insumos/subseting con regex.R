library("dplyr")

url_w4_4_a<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
url_w4_4_b<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url_w4_4_a,"week4_4_a.csv")
download.file(url_w4_4_b,"week4_4_b.csv")

week4_4_a<-read.csv("week4_4_a.csv")
week4_4_b<-read.csv("week4_4_b.csv")

head(week4_4_a)
head(week4_4_b)


data_junta<-merge(week4_4_a, week4_4_b, by.x = "X", by.y = "CountryCode")

names(data_junta)<-tolower(names(data_junta))
names(data_junta)
table(grepl("June 30", data_junta$special.notes))
grep("June 30", data_junta$special.notes, value=T)



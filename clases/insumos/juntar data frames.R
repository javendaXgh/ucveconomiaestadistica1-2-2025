url3<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
url4<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(url4,"q4.csv")
download.file(url3, "q3.csv")
q3<-read.csv("q3.csv")
q4<-read.csv("q4.csv")
library("dplyr")
library("reshape")
str(q3)
str(q4)
head(q3,8)
head(q4,8)
data_junta<-merge(q3, q4, by.x = "X", by.y = "CountryCode")
str(data_junta)
data_junta<-mutate(data_junta, Gross.domestic.product.2012 = as.numeric(as.character(Gross.domestic.product.2012)))

data_junta.f<-filter(data_junta, Gross.domestic.product.2012>0)
data_q1<- arrange(data_junta.f, desc(Gross.domestic.product.2012))
head(select(data_q1,1:4,13))
data_q3[13, 1:4]

#pregunta 4
#High income: OECD" and "High income: nonOECD
data_junta.f2<-mutate(data_junta.f, Income.Group = as.character(Income.Group))
class(data_junta.f2$Income.Group)


data_q4<-group_by(data_junta.f2, Income.Group)
class(data_q4$Income.Group)
data_q5<-summarize(data_q4, Gross.domestic.product.2012 = mean(Gross.domestic.product.2012) )
data_q5
#data_q4<-tapply(data_junta.f2$Gross.domestic.product.2012,data_junta.f2$Income.Group,mean)
head(data_q4)

#pregunta 5
data_q6<-quantile(data_junta.f2$Gross.domestic.product.2012, na.rm=T)
data_q6
data_q8<-data_junta.f2[ ,"Income.Group"]
#data_junta.f3<-mutate(data_junta.f2, q=quantile(data_junta.f2$Gross.domestic.product.2012, na.rm=T))
library(plyr)
library(Hmisc)

q

data_q7<- mutate(data_q1, quantiles=cut2(Gross.domestic.product.2012 ,g=5))
data_q7
table()

class(data_junta.f2$Income.Group)

data_q5<-mutate(data_junta.f2, cuantiles=data_q6








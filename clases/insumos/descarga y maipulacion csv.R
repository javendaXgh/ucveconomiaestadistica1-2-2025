fileURL4<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL4, destfile = "preg5.csv", mode = "wb")
DF<-read.csv("preg5.csv")
library(data.table)
DT<-data.table(DF)
DT
DT<-fread("preg5.csv")
fv<-DT[ ,pwgtp15]
a1<- rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
a2<- sapply(split(DT$pwgtp15,DT$SEX),mean)
a3<- mean(DT$pwgtp15,by=DT$SEX)
a4<- mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
a5<- DT[,mean(pwgtp15),by=SEX]
a6<- tapply(DT$pwgtp15,DT$SEX,mean)

system.time(rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2])
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(a3)


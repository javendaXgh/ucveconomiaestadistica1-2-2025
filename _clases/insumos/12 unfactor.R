library("dplyr")
library("varhandle")

url_w4_2<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(url_w4_2,"week4_2.csv")
week4_2<-read.csv("week4_2.csv")
week4_2
head(week4_2)
str(week4_2)
procesada1<-week4_2[5:194,]

head(procesada1)
names(procesada1)
str(procesada1)
columna_desfactorizada <- as.numeric(as.character(procesada1$X.3)) 
procesada2<-mutate(procesada1, X.3= unfactor(procesada1[ ,5]), na.rm=T)
head(procesada2)

class(procesada2$X.3)


procesada3<-mutate(procesada2, X.3= gsub(",","",procesada2[,5],))
procesada3$X.3

procesada4<-mutate(procesada3, X.3= as.numeric(procesada3[,5]))
head(procesada4)
class(procesada4[,5])
mean(procesada4[,5])


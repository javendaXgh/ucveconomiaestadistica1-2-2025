
n5<-as.data.frame(read.fwf("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for", 
             c(28,4,9)))
n6<-n5[5:1258,2]
n6
n6[1:6]
n8<-as.numeric(levels(n6)[n6])
sum(n8)

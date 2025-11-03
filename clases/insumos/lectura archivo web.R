fileURL2<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileURL2, destfile="exc2.xlsx", mode =  "wb") #prestar atencion al wb que hace referencia 

                                                           #a archivos binarios
w1_q1_3<-read_excel("exc2.xlsx", col_names = FALSE)
library(readxl)
w1_q1_3

chunk <- apply(w1_q1_3[18:23,7:15],1, function(x){paste(x, collapse="\t")})
#Create a new variable called dat, using the read.table function 
#with the three settings text=chunk, header=TRUE, and sep="\t".

dat<-read.table(text=chunk, header = T, sep='\t')
dat<-w1_q1_3[18:23,7:15]
sum(dat$Zip*dat$Ext, na.rm = T)

ft<-dat[dat$Zip]
dat [1,1]+dat [2,1]

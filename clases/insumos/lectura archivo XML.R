library(XML)
fileURL3<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileURL3, destfile = "arcxml.xml", mode = "wb")
doc<-xmlTreeParse("arcxml.xml",  useInternal=TRUE )
rootnode<-xmlRoot(doc)
xmlName(rootnode)
doc
doc[[1]][[1]][[1]][[1]][[2]]
zipcode<-xpathSApply(rootnode, "//zipcode",xmlValue)
zc<-as.factor( zipcode)
zipcode<-xpathSApply(rootnode, "//li[@class= 'zipcode']", xmlValue)
zipcode
dim(zipcode)
zipcode<- rootnode[[1]][[3]][[2]]
zipcode
dim(rootnode)
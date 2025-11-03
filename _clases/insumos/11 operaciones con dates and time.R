
x<- as.Date("2017-01-05")
unclass(x)

weekdays(x)

x <- Sys.time()
x
#[1] "2016-06-28 09:00:05 EDT"
class(x) 
## 'POSIXct' object
#[1] "POSIXct" "POSIXt"

#ver la metadata
p<- as.POSIXlt(x)
names(unclass(p))

#[1] "sec" "min" "hour" "mday" "mon" "year" "wday"
#[8] "yday" "isdst" "zone" "gmtoff"


#You can also use the POSIXct format.
x <- Sys.time()
x ## Already in 'POSIXct' format
#[1] "2016-06-28 09:00:05 EDT"
unclass(x) ## Internal representation
#[1] obtengo el numero
x$sec ## Can't do this with 'POSIXct'!
#Error in x$sec: $ operator is invalid for atomic vectors
p <- as.POSIXlt(x)
p$min ## That's better




###

datestring <- c("January 10, 2012 10:40", "December 9, 2011 9:10")
x <- strptime(datestring, "%B %d, %Y %H:%M")
x
##[1] "2012-01-10 10:40:00 EST" "2011-12-09 09:10:00 EST"
class(x)
##[1] "POSIXlt" "POSIXt"
unclass(x)
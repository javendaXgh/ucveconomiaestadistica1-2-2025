corr<- function (directory, threshold= 0){
    files_list <- list.files("specdata", full.names=TRUE)
    fre<- data.frame()
    for (i in (1:332) {
        fre <- rbind(fre, read.csv(files_list[i]))
    }
    
    fra<-!is.na(read.csv(files_list[directory])[ ,c(2,3)])
    numrow<-nrow(fra)
    if (for j in (1:numrow){
        
    }
    gri<-gra[ ,2]
    #gru<-gri[!is.na(gri)]
    gre<-gra[ ,3]
    #gro<-gre[!is.na(gri)]
    endi<-cor(gri, gre, use="complete.obs")
    print( paste ((gro), (gru)))
    print(endi)
    head(endi)
}
corr(150)

pollutantmean<- function (specdata, pollutant, id=332) {
    files_list <- list.files("specdata", full.names=TRUE)
    dat <- data.frame()                             
    for (i in (id[1]):(id[length(id)])) {  
        #for (i in 1:332) {                                
        
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    
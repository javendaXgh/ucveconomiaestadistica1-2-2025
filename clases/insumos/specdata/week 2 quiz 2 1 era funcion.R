#ver los archivos contenidos en specdata
list.files("specdata")
#resumen de specdata, que no aporta nada ya que esta consultando solo el nombre del fichero
summary("specdata")

# inicio procedimiento para crear un data frame con todos los archivos
# indico que al obtener el listado de ficheros incluyan la ruta de acceso dentro del 
#directorio de R
files_full<-list.files("specdata", full.names = TRUE)
files_full

# crear una data frame vacio
dataall<-data.frame()

for (i in 1:332){
    dataall<- rbind(dataall, read.csv(files_full[i]))
}



#con una funcion

pollutantmean2<- function (specdata, pollutant, id){
    files_list <- list.files("specdata", full.names=TRUE)   #creates a list of files
    dat <- data.frame()                             #creates an empty data frame
    for (i in 1:332) {                                
        #loops through the files, rbinding them together 
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    
    dat_subset <-dat[which(dat[, "ID"] == id),]
    if (pollutant == "nitrate"){
        mean(dat_subset[, "nitrate"], na.rm=TRUE)
    } else {
        mean(dat_subset[, "sulfate"], na.rm=TRUE)
    }
}


pollutantmean<- function (specdata, pollutant, id) {
    files_list <- list.files("specdata", full.names=TRUE)
    dat <- data.frame()                             
    for (i in (id[1]):(id[length(id)])) {                                
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    
    #dat <- read.csv(files_list[id[1]:id[length(id)]])
    
    if (pollutant == "nitrate"){
        mean(dat[, "nitrate"], na.rm=TRUE)
    } else {
        mean(dat[, "sulfate"], na.rm=TRUE)
    }
}


complete<-function(specdata, id) {
    files_list <- list.files("specdata", full.names=TRUE)
    fra<- data.frame()
    print("id nobs")
    
    for (i in (id[1]):(id[length(id)])) {
        counter<-0
        fra<-!is.na(read.csv(files_list[i])[ ,c(2,3)])
        number_row<-(nrow(fra))
        number_col<-(ncol(fra))
    
        var<- (read.csv(files_list[i])[1,4])

        for (h in (1:number_row)) {
            tre<- (h)
            if (((fra[tre,1]) & (fra[tre,2]))==T) {
            counter<-counter+1
            }
        }
        print( paste ( (var), (counter)))
        #print (var)
        #print (counter)
    }
    
}

complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 1:2)
complete("specdata", 30:25)
complete("specdata", 200)


pollutantmean("specdata", "nitrate", c(23,3,2))
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", c(70,71,72))
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

pollutantmean<- function (specdata, pollutant, id=332) {
    files_list <- list.files("specdata", full.names=TRUE)
    dat <- data.frame()                             
    for (i in (id[1]):(id[length(id)])) {  
    #for (i in 1:332) {                                
            
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
    idl<-length(id)
    mtx<- matrix(NA,0,2)
    cn<-c("id","nobs")
    colnames(mtx)<-cn
    tm<-Sys.time()
    
        for (i in seq_along (id)){
        counter<-0
        sd<- (id[i])
        fra<-!is.na(read.csv(files_list[sd])[ ,c(2,3)])
        number_row<-(nrow(fra))
        number_col<-(ncol(fra))
        var<- (read.csv(files_list[sd])[1,4])

            for (h in (1:number_row)) {
                tre<- (h)
                if (((fra[tre,1]) & (fra[tre,2]))==T) {
                    counter<-counter+1
                }
                if (h==number_row){
                    mtx<-rbind (mtx, (c(var,counter)))
                    
                }
            
            }
   
        }
    tf<-Sys.time()
    tt<-tf-tm
    print(mtx)
    print (tt)
}


complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 1:332)
complete("specdata", 200)


pollutantmean("specdata", "nitrate", c(23,3,2))
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", c(70,71,72))
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate", 1:332)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)



set.seed(42)
cc<-complete("specdata", 332:1)
use<-sample (332,10)
print(cc [use,"nobs"])

debug(complete)



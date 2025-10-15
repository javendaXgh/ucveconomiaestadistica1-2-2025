rm(list=ls())
library(readxl)
library(dplyr)
getwd()
valdol<-16
setwd("C:/alterno/Google Drive/formacion/Bussines analitycs/bases de datos")
zonaprop<- read_excel ("ZonaProp.xlsx")
zonaprop$titulo<-tolower(zonaprop$titulo)
zonaprop$descripcion<-tolower(zonaprop$descripcion)
zonaprop$titulo<-gsub("ñ","nnn", zonaprop$titulo)
zonaprop$descripcion<-gsub("ñ","nnn", zonaprop$descripcion)
zonaprop$banos<-gsub("ñ","nnn", zonaprop$banos)


#ad ID columm
id<-seq(1,dim(zonaprop)[1])

#pasar  a factor: barrio, tipo, cod_pos

#convertir  a numeros: precio, banos, expensas, antiguedad (pareciera solo llegar hasta 99 dias)

zonaprop<-transform(zonaprop, id=id, ambientes= factor(ambientes), barrio=factor(barrio), 
                    cod_zp=factor(cod_zp), tipo= factor(tipo)) 
#subset de columnas
zonaprop1<-zonaprop[,-c(1,19,20,21)]
precios1<-zonaprop$precio
precios1<-gsub("\\.","",precios1)
zonaprop1<-mutate(zonaprop1, precio=gsub("\\.","",precios1))

#subset de precios
#los sin precio
zonapropsinprecio<-zonaprop1[!grepl("\\$",zonaprop1$precio),]


zonaprop2<-zonaprop1[grepl("\\$",zonaprop1$precio),]

zonaprop3dolares<-zonaprop2[grepl("U\\$S",zonaprop2$precio),]
dolares<-zonaprop3dolares$precio
dolares<-gsub("U\\$S ","",dolares)
dolares<-as.numeric(dolares)
zonaprop3dolares<-mutate(zonaprop3dolares,precio=dolares)


zonaprop4pesos<-zonaprop2[!grepl("U\\$S",zonaprop2$precio),]
pesos<-zonaprop4pesos$precio
pesos<-gsub("\\$ ","",pesos)
pesos<-as.numeric(pesos)
zonaprop4pesos<-mutate(zonaprop4pesos, precio=pesos/valdol)

zonaprop<-rbind(zonaprop4pesos,zonaprop3dolares)

#subset banos
zonaprop_dpto<-zonaprop[grepl("Departamento",zonaprop$tipo),]
zonaprop_dpto_cb<-zonaprop_dpto[grepl("",zonaprop_dpto$bano),]
zonaprop_dpto_sb<-zonaprop_dpto[!grepl("",zonaprop_dpto$bano),]

#buscar en los sin bano el bano en otras columnas
bano_bus_t<-zonaprop_dpto_sb [grepl("bannno",zonaprop_dpto_sb$titulo),]
bano_bus_d<-zonaprop_dpto_sb [grepl("bannno",zonaprop_dpto_sb$descripcion),]


x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5)
substring(x, 2, 4:6)
substring(x, 2) <- c("..", "+++")
x

a <- "DP=26;AN=2;DB=1;AC=1;MQ=56;MZ=0;ST=5:10,7:2;CQ=SYNONYMOUS_CODING;GN=NOC2L;PA=1^1:0.720&2^1:0"
m = regexpr("bannno completo",bano_bus_d$descripcion)
z<-substr(bano_bus_d$descripcion,m-2,m+attr(m,"match.length")-7)
z
p<-bano_bus_d$descripcion
substr(a,m+3,m+attr(m,"match.length")-2)
p[341:345]

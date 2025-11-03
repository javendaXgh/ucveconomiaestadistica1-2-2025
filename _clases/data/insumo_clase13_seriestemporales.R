library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(readxl)
library(tidyverse)

# temperatura <- read.csv('/Users/josemiguelavendanoinfante/R/UCV_ECONOMIA_R4DS/scripts/data_in/GlobalLandTemperaturesByCity.csv')
# rm(temperatura)
# 
# temp_venezuela <- temperatura%>%
#   filter(Country=='Venezuela')

# saveRDS(temp_venezuela,'clases/data/temp_venezuela.rds')

temp_venezuela <- readRDS('data/temp_venezuela.rds')

temp_mara_cuma <- temp_venezuela%>%
  filter(City %in% c('Maracaibo','Cumaná','Caracas',"San Cristóbal"))%>%
  # filter(between( dt,'2012-01-01','2012-12-01'))%>%
  mutate(mes=month(dt))%>%
  select(dt,AverageTemperature, City, mes)%>%
  rename(Ciudad= City)


temperatura_ccs <- temp_mara_cuma%>%
  filter(Ciudad =='Caracas')%>%
  filter(between( dt,'2008-01-01','2012-12-01'))%>%
  select(-Ciudad)%>%
  mutate(year= year(dt))%>%
  mutate(fecha=as_date(dt))


ccs_linea <- ggplot(temperatura_ccs, 
       aes(x=fecha, 
           y=AverageTemperature))+  
  geom_line( color='red',
             size = 1)

# ccs_linea%>%
#   ggplotly()


ccs_gr <- ggplot(temperatura_ccs, 
       aes(x=factor(mes), 
           y=AverageTemperature,
           color=year,
           group = year))+  
  geom_line( size = 1)

# ccs_gr%>%
#   ggplotly()

temperatura_mcbo <- temp_mara_cuma%>%
  filter(Ciudad =='Maracaibo')%>%
  filter(between( dt,'2008-01-01','2012-12-01'))%>%
  select(-Ciudad)%>%
  mutate(year= year(dt))


mcbo_gr <- ggplot(temperatura_mcbo, 
       aes(x=factor(mes), 
           y=AverageTemperature,
           color=year,
           group = year))+  
  geom_line( size = 1)

# mcbo_gr%>%
#   ggplotly()

####### inpc
# download.file('https://www.bcv.org.ve/sites/default/files/precios_consumidor/4_5_7_0.xls',
#               'INPC.xls')


INPC <- read_excel('INPC.xls')%>%
  slice(-1:-6)

# Nombres a columnas
names(INPC) <- c('fecha','indice','var')

meses_es <- c("enero", "febrero", "marzo", 
              "abril", "mayo", "junio",
              "julio", "agosto", "septiembre", 
              "octubre", "noviembre", "diciembre")


data_inpc <- INPC%>%
  mutate(fecha= str_remove_all(fecha,'[:punct:]'),
         fecha= str_to_lower(fecha),
         indice= as.numeric(indice),
         var= as.numeric(var))%>%
  mutate(year_extract= ifelse(str_detect(fecha,'^[:digit:]'),fecha,NA ))%>%
  mutate(year_extract= as.numeric(year_extract))%>%
  fill(year_extract, .direction = "down")%>%
  filter(!is.na(indice))%>%
  mutate(num_mes= match(fecha, meses_es),
         var2= (indice/lead(indice)-1)*100,
         fecha_nva=paste0(year_extract,'-',num_mes,'-01'),
         fecha_nva=as_date(fecha_nva))%>%
  select( fecha_nva, indice, var2, var)%>%
  mutate(year=year(fecha_nva),
         mes= month(fecha_nva))

View(data_inpc)

ipc_linea <- ggplot(data_inpc, 
       aes(x=fecha_nva, 
           y=var2))+  
  geom_line( color='red',
             size = 1)

# ipc_linea%>%
#   ggplotly()

var_inpc <- ggplot(data_inpc%>%
         filter(year<2015), 
       aes(x=factor(mes), 
           y=var2,
           color=year,
           group = year))+  
  geom_line( size = 1)

var_inpc%>%
  ggplotly()

    
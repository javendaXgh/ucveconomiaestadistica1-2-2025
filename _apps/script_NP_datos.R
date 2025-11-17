# Datos extraidos de Banco Mundial 
# install.packages('WDI')
library(WDI)

# CHINA su abreviatura para el Banco Mundial es "CN"

indicadores <- WDIsearch("monetary") # se hizo busqueda de todos los indicadores
# relacionados a lo "monetario"

class(indicadores)
names(indicadores)
indicadores$indicator
View(indicadores)

datos <- WDI(
  country = "CN",
  indicator = c("NY.GDP.MKTP.CD", # PIB $US a precios actuales (nominal)
                # "SP.POP.TOTL", # población total 
                # "NY.GDP.PCAP.CD", # PIB per cápita ($US actuales)
                # "FM.LBL.BMNY.GD.ZS", # Masa monetaria M2 (% del PIB)
                # "FI.RES.TOTL.CD", # Reservas de divisas (incluye oro, US$ actuales)
                # "FR.INR.LEND", # Tasa de interés interbancaria "corto plazo"
                # "GFDD.DI.14", # Dinero móvil / transacciones digitales
                # "FB.CBK.BWRK.P3",# Cajeros automáticos por cada 1000 habitantes 
                # "FM.AST.PRVT.GD.ZS", # Crédito del sector monetario al sector privado (% del PIB)
                # "FM.LBL.QMNY.CN.WB", # Pasivos cuasimonetarios (local)
                # "GB.DOD.DMSY.CN", # Deuda del gobierno central, crédito del sistema monetario (moneda local actual)
                # "GB.FIN.DMSY.CN", # Financiamiento interno, crédito del sistema monetario (moneda local actual)
                # "GPSS_1", # Cuentas de dinero electrónico por cada 1,000 adultos
                # "i_mob_transactions_number_pop", # Transacciones de dinero móvil por cada 100,000 adultos
                # "fin13a", # Usó cuenta móvil ≥2 veces al mes (% de 15+ años)
                # "fin13a.s", # Usó cuenta móvil ≥2 veces al mes (% con cuenta móvil, 15+ años)
                # "fin13b", # Almacenó dinero en cuenta móvil (% de 15+ años)
                # "fin13b.s", # Almacenó dinero en cuenta móvil (% con cuenta móvil, 15+ años)
                # "fin14a1.d", # Used a mobile phone or the internet to send money (% age 15+)
                # "fin14a1.d.9", # Used a mobile phone or the internet to send money, rural (% age 15+)
                # "fin13.1f", # Razón para no tener una cuenta de dinero móvil: no tiene su propio teléfono móvil (% de 15+ años)
                "fin13.1f.s"), # Razón para no tener una cuenta de dinero móvil: no tiene su propio teléfono móvil (% de personas sin cuenta, 15+ años)
  start = 2014,
  end = 2024, 
  extra = FALSE, 
  cache = NULL, 
  latest = NULL, 
  language = "en"
)

indicadores_seleccionados <- c("SP.POP.TOTL", # población total
                               "NY.GDP.PCAP.CD", # PIB per cápita ($US actuales)
                               "FM.LBL.BMNY.GD.ZS")

datos2 <- WDI(
  country = "CN",
  indicator = indicadores_seleccionados,
  start = 2014,
  end = 2024, 
  extra = FALSE, 
  cache = NULL, 
  latest = NULL, 
  language = "en"
)

View(datos2)
datos2


############
# install.packages("owidapi")
library(owidapi)
owid_get("automated-teller-machines-atms-per-100000-adults", entities = c("CHN"))

###########
indicadores_NP <- c("NY.GDP.MKTP.CD", # PIB $US a precios actuales (nominal)
                    "SP.POP.TOTL", # población total
                    "NY.GDP.PCAP.CD", # PIB per cápita ($US actuales)
                    "FM.LBL.BMNY.GD.ZS", # Masa monetaria M2 (% del PIB)
                    "FI.RES.TOTL.CD", # Reservas de divisas (incluye oro, US$ actuales)
                    "FR.INR.LEND", # Tasa de interés interbancaria "corto plazo"
                    "GFDD.DI.14", # Dinero móvil / transacciones digitales
                    "FB.CBK.BWRK.P3",# Cajeros automáticos por cada 1000 habitantes
                    "FM.LBL.QMNY.CN.WB", # Pasivos cuasimonetarios (local)
                    "GPSS_1", # Cuentas de dinero electrónico por cada 1,000 adultos
                    "i_mob_transactions_number_pop", # Transacciones de dinero móvil por cada 100,000 adultos
                    "fin13a", # Usó cuenta móvil ≥2 veces al mes (% de 15+ años)
                    "fin13b", # Almacenó dinero en cuenta móvil (% de 15+ años)
                    "fin14a1.d", # Usó un teléfono móvil o internet para enviar dinero (% de personas de 15+ años)
                    "fin14a1.d.9" )# Usó un teléfono móvil o internet para enviar dinero, zona rural (% de personas de 15+ años)



######################
##########################################################################################
# usar paquete wbids
# Datos del Banco Mundial obtenidos desde la API de la institución
## wbids World Bank International Debt Statistics
# https://datacatalog.worldbank.org/search/dataset/0038015
# install.packages('wbids')
#https://cran.r-project.org/web/packages/wbids/wbids.pdf
##########################################################################################

##########################################################################################
######## Cargar librerías                                                          #######
##########################################################################################
library(tidyverse)
library(wbids)

##########################################################################################
######## Exporar Contenido                                                         #######
##########################################################################################
# Contrapartes (países)
View(ids_list_counterparts()) 

# Id Venezuela
ids_list_counterparts()%>%
  filter(str_detect(counterpart_name,'Vene'))%>%
  pull(counterpart_id)

# listar geografías (datos país y regiones)
View(ids_list_geographies())
names(ids_list_geographies())


latam <- ids_list_geographies()%>%
  filter(str_detect(region_name,'Latin'))
View(latam)

# obtener códigos países latinoamérica
latam_geographies <- ids_list_geographies()%>%
  filter(str_detect(region_name,'Latin'))%>%
  pull(geography_id)

# obtener códigos países 
codigos_paises_mundo <- ids_list_geographies()%>%
  filter(geography_type=='Country')%>%
  pull(geography_id)

# consultar listado de series disponibles
ids_list_series()#series disponibles

View(ids_list_series())

# Ver series relacionadas con deuda
View(ids_list_series()%>%
       filter(str_detect(series_name, 'debt|DEBT|Debt'))%>%
       select(series_id, series_name))

dim(ids_list_series())

#  consultar el nombre de una serie en particular
ids_list_series()%>%
  filter(series_id=="FI.RES.TOTL.DT.ZS")%>%
  select(series_name)


##########################################################################################
######## Descarga de dos series en loop por país de latinoamérica (latam):        #######
# "FI.RES.TOTL.CD" Total reserves (includes gold, current US$)
# "FI.RES.TOTL.DT.ZS" Total reserves (% of total external debt)

# se usa la función ids_get() para la descarga de los datos
##########################################################################################



# Incializar las data frames
# df_reserva_int_latam <- tibble()
# df_res_perc_gdp_latam <- tibble()

# 
# 1:10+1
# 
# for(i in 1:10){
#   for(j in 1:10){
#     print(paste(i,'X',j,'=',i*j))
#   }
#   # print(i)
# }

for(i in latam_geographies){
  print(i)
}



indicadores_NP <- c("NY.GDP.MKTP.CD", # PIB $US a precios actuales (nominal)
                    "SP.POP.TOTL", # población total
                    "NY.GDP.PCAP.CD", # PIB per cápita ($US actuales)
                    "FM.LBL.BMNY.GD.ZS", # Masa monetaria M2 (% del PIB)
                    "FI.RES.TOTL.CD", # Reservas de divisas (incluye oro, US$ actuales)
                    "FR.INR.LEND", # Tasa de interés interbancaria "corto plazo"
                    "GFDD.DI.14", # Dinero móvil / transacciones digitales
                    "FB.CBK.BWRK.P3")
indicadores_NP2 <- c("NY.GDP.MKTP.CD", # PIB $US a precios actuales (nominal)
                     "SP.POP.TOTL", # población total
                     "NY.GDP.PCAP.CD", # PIB per cápita ($US actuales)
                     "FM.LBL.BMNY.GD.ZS", # Masa monetaria M2 (% del PIB)
                     "FI.RES.TOTL.CD", # Reservas de divisas (incluye oro, US$ actuales)
                     "FR.INR.LEND", # Tasa de interés interbancaria "corto plazo"
                     "GFDD.DI.14", # Dinero móvil / transacciones digitales
                     "FB.CBK.BWRK.P3",# Cajeros automáticos por cada 1000 habitantes
                     "FM.LBL.QMNY.CN.WB", # Pasivos cuasimonetarios (local)
                     "GPSS_1", # Cuentas de dinero electrónico por cada 1,000 adultos
                     "i_mob_transactions_number_pop", # Transacciones de dinero móvil por cada 100,000 adultos
                     "fin13a", # Usó cuenta móvil ≥2 veces al mes (% de 15+ años)
                     "fin13b", # Almacenó dinero en cuenta móvil (% de 15+ años)
                     "fin14a1.d", # Usó un teléfono móvil o internet para enviar dinero (% de personas de 15+ años)
                     "fin14a1.d.9" )# Us

df_test <- ids_get( geographies = "CHN",
                    series = indicadores_NP2[15],
                    start_year = 1970,
                    end_year = 2025,
                    progress = TRUE
)

View(df_test)


read.csv('apps/data.csv')




########
install.packages("webshot2")
# Or, install the latest development version from GitHub
# remotes::install_github("rstudio/webshot2")
# Load the package
library(webshot2)

webshot("https://ourworldindata.org/grapher/average-harmonized-learning-outcome-scores?tab=table")

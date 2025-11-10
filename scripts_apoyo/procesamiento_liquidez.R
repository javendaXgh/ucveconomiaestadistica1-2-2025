library(tidyverse)
library(readxl)

hojas <- readxl::excel_sheets('data/liquidez_monetaria_semanal1.xls')


extraccion_liquidez <- function(libro,
                                hoja){
  liquidez <- read_excel(libro,
                         col_types = "text",
                         sheet = hoja )
  
  col_nombres <- apply(liquidez[3:4,1:8],2,paste,collapse=' ')
  names(col_nombres) <- NULL
  
  col_nombres <- str_remove(col_nombres,'^NA ')
  colnames(liquidez) <- col_nombres
  
  liquidez%>%
    slice(-1:-5)%>%
    janitor::clean_names()%>%
    filter(!is.na(monedas_y_billetes_1))%>%
    rename(fecha_sucia=1)%>%
    mutate(fecha_sucia= str_remove_all(fecha_sucia,' \\(\\*\\)|\\*'))%>%
    # filter(!is.na(fecha_sucia))%>%
    mutate(fecha= ifelse(nchar(fecha_sucia)==5,
                         as.Date(as.numeric(fecha_sucia), origin = "1899-12-30"),
                         as.Date(fecha_sucia, format = "%d/%m/%Y")),
           fecha=as.Date(fecha))%>%
    select(-fecha_sucia)%>%
    mutate(across(where(is.character), as.numeric))%>%
    select(8,1:7)%>%
    mutate(mes= month(fecha),
           semana= week(fecha),
           year=year(fecha))%>%
    mutate(variacion_percent=round(variacion_percent,2))%>%
    filter(!is.na(fecha))
}


dfliqui1 <- extraccion_liquidez('data/liquidez_monetaria_semanal1.xls',
                           hojas[1])

dfliqui2  <- extraccion_liquidez('data/liquidez_monetaria_semanal1.xls',
                           hojas[2])

dfliqui3  <- extraccion_liquidez('data/liquidez_monetaria_semanal1.xls',
                           hojas[3])

df_liquidez <- bind_rows(dfliqui1,
                         dfliqui2,
                         dfliqui3)%>%
  arrange(desc(fecha))%>%
  mutate(variacion= (liquidez_monetaria/lead(liquidez_monetaria)-1)*100,
         variacion= round(variacion,2),
         # verificar si existen diferencias entre variaciones porcentuales
         # del libro de excel con las calculadas
         diferencia_check= variacion_percent-variacion)%>%
  # se excluyen casos donde existan diferencias entre variaciones
  filter(diferencia_check==0)%>%
  select(-diferencia_check)


gr_liquides <- ggplot(df_liquidez%>%
                        filter(fecha>min(dfliqui1$fecha)), 
                    aes(x=fecha, 
                        y=liquidez_monetaria))+  
  geom_line( color='red',
             size = 1)

gr_liquides

View(df_liquidez)


########
INPC <- read_excel('INPC.xls')%>%
  slice(-1:-6)

# Nombres a columnas
names(INPC) <- c('fecha','indice','var')

meses_es <- c("enero", "febrero", "marzo", 
              "abril", "mayo", "junio",
              "julio", "agosto", "septiembre", 
              "octubre", "noviembre", "diciembre")


df_inpc <- INPC%>%
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
  mutate(mes= month(fecha_nva),
         semana= week(fecha_nva),
         year=year(fecha_nva))%>%
  filter(fecha_nva>min(dfliqui1$fecha)&fecha_nva<max(dfliqui1$fecha) )%>%
  rename(fecha= fecha_nva)

df_join <- left_join(df_inpc,
          df_liquidez,
          by=c('year','semana'))%>%
  select(fecha.x, indice, var2,liquidez_monetaria, variacion, mes.x)%>%
  rename(var_inpc= var2,
         var_liquid= variacion,
         fecha=fecha.x)
  
View(df_join)



ggplot(data= df_join,
       aes(x= liquidez_monetaria,
           y= indice,
           colour = mes.x))+
  geom_point( size=2)+
  # scale_x_continuous(transform='log') +
  # scale_y_continuous(transform='log2')+
  geom_smooth(method='lm')+
  labs(title='Relación Liquidez Monetaria - Índice de Precios',
       x='Liquidez Monetaria',
       y='Índice Nacional de Precios al Consumidor')

write.csv(df_join, 'data/liquidez_inpc.csv', row.names = FALSE)

plot(x= df_join$liquidez_monetaria,
     y= df_join$indice)

abline(lm(df_join$indice ~ df_join$liquidez_monetaria))


model <- lm(df_join$indice ~ df_join$liquidez_monetaria)
#get list of residuals 
res <- resid(model)

plot(df_join$liquidez_monetaria,
     res, 
     ylab="Residuales",
     xlab="liquidez_monetaria", 
     main="Monto Liquidez prediccion inflación") 

abline(0, 0)                  # the horizon
View(res)

res[2]
#produce residual vs. fitted plot
plot(fitted(model), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res) 


#Create density plot of residuals
plot(density(res))

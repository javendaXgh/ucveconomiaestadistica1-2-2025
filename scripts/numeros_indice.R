# indice simple
## obtener datos produccion petroleo
prod_petroleo_ven <- read_csv('data/petroleum-production-col_ven.csv')
View(head(prod_petroleo_ven))

prod_petroleo <- prod_petroleo_ven%>%
  # filter(Entity=='Venezuela')%>%
  rename(produccion= 3)%>%
  mutate(produccionK=produccion/1000)
options(scipen = 999)

write_csv(prod_petroleo,'data/producion_petroleo.csv')

ggplot(prod_petroleo,aes(x=Year,
                              y=produccionK, 
                              color=Entity ))+
  geom_line()

### series temporales
# suavizado para series temporales
library(pdfetch)
library(tidyverse)
library(xts)
valor_apple <- pdfetch_YAHOO(c("AAPL"))

valor_apple <- valor_apple%>%
  as_data_frame()%>%
  mutate(accion= 'Apple',
         codigo= 'AAPL')%>%
  bind_cols( fecha= index(valor_apple))

df_valor_apple <- valor_apple%>%
  mutate(variacion_precio=AAPL.close/lag(AAPL.close))%>%
  select(fecha, AAPL.close,variacion_precio,AAPL.volume )%>%
  mutate(mes= month(fecha),
         year= year(fecha),
         dia= yday(fecha),
         mes_year= paste0(mes,'-',year))%>%
  filter(year>2020)


write_csv(df_valor_apple,'data/df_valor_apple.csv')
ggplot(df_valor_apple, 
       aes(x=dia,
           y=AAPL.close,
           # group = year,
           color=factor(year)))+
  geom_line()+
  geom_smooth()

ggplot(df_valor_apple1, 
       aes(x=fecha,
           y=AAPL.close#,
           # group = year,
           # color=factor(year)
           ))+
  geom_line()+
  geom_smooth()

df_valor_apple1 = read.csv('https://raw.githubusercontent.com/javendaXgh/ucveconomiaestadistica1/refs/heads/main/data/df_valor_apple.csv')%>%
  as_tibble()%>%
  mutate(fecha= as.Date(fecha))

ggplot(df_valor_apple, 
       aes(x=fecha,
           y=AAPL.close))+
  geom_line()

View(valor_apple)
valor_apple_pr <- valor_apple%>%
  
  
  mutate(mes= month(fecha),
         year= year(fecha))


valor_apple_pr
names(valor_apple)

ggplot(valor_apple_pr, 
       aes(x=fecha,
           y=variacion_precio,
           # group = c(year,mes),
           color= as.factor(year)))+
  geom_line()


#####
# Ejemplo de tu dataframe (reemplaza esto con tu dataframe real)
# Ejemplo de tu dataframe (reemplaza esto con tu dataframe real)
set.seed(123)
df <- data.frame(
  fecha = seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "day"),
  valor = runif(1461, min = 0, max = 100)
)
df <- df %>%
  mutate(
    anio = year(fecha),
    dia_del_anio = yday(fecha)
  )
ggplot(df, aes(x = dia_del_anio, y = valor, color = as.factor(anio))) +
  geom_line() +
  labs(x = "Día del año", y = "Valor", color = "Año") +
  theme_minimal()

library(plotly)
grafico= ggplot(df_valor_apple,
                aes(x=dia,
                    y=AAPL.close,
                    color=factor(year)))+
  geom_line()+
  geom_smooth() # permite ver la serie suavizada y es recomendable para visualizar tendencias

grafico%>%
  ggplotly()

##########
library(readxl)
library(tidyverse)
library(lubridate)
download.file('https://www.bcv.org.ve/sites/default/files/precios_consumidor/4_5_7_0.xls','INPC.xls')



# View(tdc)
INPC <- read_excel('INPC.xls')%>%
  slice(-1:-6)

# View(INPC)
# apply(INPC,2, class)

# formatos
names(INPC) <- c('fecha','indice','var')

meses_es <- c("enero", "febrero", "marzo", 
              "abril", "mayo", "junio",
              "julio", "agosto", "septiembre", 
              "octubre", "noviembre", "diciembre")

# https://rstudio.github.io/cheatsheets/strings.pdf

# INPC%>%
  # print(n=15)

df_inpc <- INPC%>%
  mutate(fecha= str_remove_all(fecha,'[:punct:]'),
         fecha= str_to_lower(fecha),
         indice= as.numeric(indice),
         var= as.numeric(var))%>%
  mutate(year_extract= ifelse(str_detect(fecha,'^[:digit:]'),fecha,NA ))%>%
  fill(year_extract, .direction = "down")%>%
  filter(!is.na(indice))%>%
  mutate(num_mes= match(fecha, meses_es),
         var2= (indice/lead(indice)-1)*100,
         fecha_nva=paste0(year_extract,'-',num_mes,'-01'),
         fecha_nva=as_date(fecha_nva))%>%
  select( fecha_nva, indice, var2, var)%>%
  print(n=15)

saveRDS(df_inpc,'data/INPC.rds')


library(DT)



datatable(df_inpc%>%
            mutate(var2= round(var2,1),
                   indice=round(indice,0))%>%
            select(fecha_nva,indice,var2)%>%
            rename(variacion=var2,
                   índice= indice,
                   fecha= fecha_nva),
          options = list(
            dom = 'lrtip',
            pageLength = 4,
            autoWidth = FALSE,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#053C5E','color': '#fff'});",
              "}"),
            rownames= FALSE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')#,
            ))


library(ggplot2)

# Define la ecuación de la línea recta: y = mx + c
m <- .85  # Pendiente
c <- 10  # Intercepto

# Define el rango de valores para x
x_min <- 0
x_max <- 20

# Crea un data frame con los valores de x dentro del rango
df <- data.frame(x = seq(x_min, x_max, length.out = 10))

# Calcula los valores de y correspondientes
df$y <- m * df$x + c

df <- df%>%
  mutate(x=round(x,2),
         y=round(y,2))

# Crea el gráfico con ggplot2
ggplot(df, aes(x = x, y = y)) +
  geom_line(color='#c1121f', linewidth=3) +             # Dibuja la línea
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + # Eje y
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") + # Eje x
  labs(title = "Función de Costo Lineal",
       x = "cantidades producidas",
       y = "costo total") +
  theme_bw() +             # Tema con fondo blanco y líneas oscuras
  scale_x_continuous(limits = c(x_min, x_max)) + # Define los límites del eje x
  scale_y_continuous(limits = c(0, 30)) + # Define los límites del eje x
  coord_fixed(ratio = 1)   # Hace que la escala de x e y sea la misma (opcional)

#c= costo fijo
#m= costo marginal

ggplot(data=cars, aes(x=speed,y= dist))+
  geom_point(color='#c1121f')+
  labs(title = "Velocidad ~ Distancia",
       x = "velocidad",
       y = "distancia") +
  theme_bw()

ggplot(data=cars, aes(x=speed,y= dist))+
  geom_point(color='#c1121f')+
  # stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x, se=FALSE)+
  labs(title = "Conjunto Cars: Velocidad ~ Distancia",
       x = "velocidad",
       y = "distancia") +
  theme_bw()

df_exp <- data.frame(x=1:30)%>%
  mutate(y=x^1.7)%>%
  mutate(z=runif(y,y*0.8,y*1.2 ))

# runif(1,-1,20)

ggplot(data=df_exp, aes(x=x,y= y))+
  geom_line(color='#c1121f', linewidth=3)+
  geom_point(aes(y=z),color='blue', size=2)

install.packages('ISLR')
library(ISLR)
library(openintro)
ISLR::Wage
helmet
loan50
loans_full_schema
mammals
rosling_responses
plot(x=mammals$body_wt,y= mammals$brain_wt)

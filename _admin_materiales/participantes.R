library(tidyverse)

datos_nombres <- read_csv('_admin_materiales/nombres.csv')
nombres <- datos_nombres$Nombre

ci <- datos_nombres$Cedula

participantes <- tibble(Nombre= str_to_title(nombres),
                        ci= ci,ci2=ci)%>%
  arrange(nombres)%>%
  mutate(ci=format(ci, nsmall=0, big.mark=".", decimal.mark=','),
         `N.`= 1:nrow(.))%>%
  select(`N.`, Nombre, ci,ci2)%>%
  rename(`cédula de identidad`=ci)

###CORRECCION, pruebas

df_comparacion = cbind(dfoperational,registro$mm)
names(df_comparacion)[1] <- "Fecha"
names(df_comparacion)[2] <- "Operational"
names(df_comparacion)[3] <- "Observado"


library(invgamma)
library(extraDistr)
#library(nimble)
v = pgamma(5,scale=4.77,shape=1.03) # aca usar parámetros reforecast
vv = qgamma(v,scale=11.11,shape=0.96) #aca usar parametros observado por c/mes
#vv = extraDistr::pinvgamma(v,alpha=11.11,alpha=0.96) #observado


######### INICIO DEL CODIGO ###########
load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/statr_reforecast.RData")
load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/operational_operational_12days.RData")

geopuntos = c(1,2,3,4,5,6,7,8,9)
anos = c(2011,2012,2013,2014,2015,2016)
meses = c(1,2,3,4,5,6,7,8,9,10,11,12) #ojo por que parte en mes 3 !!!!

for (k in geopuntos) {
  for (ano in anos) {
    for (mes in meses) {
      #corregir y appendear
    df = #crear un dataframe vacío
    aux = get(paste("OPERATIONAL",i,"[",la,",",lo,"]",sep="")) %>%
      dplyr::filter(month(`BRCK[[la, lo]]`) == i) %>%
    }
  }
}
##########

library(invgamma)
library(extraDistr)
load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/operational_operational_12days.RData")
mes = 7


data_operational =
  OPERATIONAL[[nearest_station]] %>%
    dplyr::filter(month(k2)==mes()) %>%
    dplyr::select(mm)

v = pgamma( data_operational(),scale=scale.reforecast(),shape=shape.reforecast() ) 


operational_corregido = qgamma( v(),scale=scale.observado(),shape=shape.observado() )

registro <- 
  hest %>%
    dplyr::select(DateTime,dplyr::contains(e())) %>%
    dplyr::mutate(m = month(DateTime)) %>%
    dplyr::filter(m == mes()) %>%
    dplyr::filter(get(e()) > tol) %>%
    dplyr::select(contains(e()))

obsprecip <- sort(unlist(as.list(registro())))



#####################################################################################################
############################ BRICK MAKER ############################################################
#.libPaths(new = "/home/forever/R/x86_64-pc-linux-gnu-library/3.4")

# Este código crea una matriz multidimensional a partir de archivos que entrega Catcher.R
# reemplazar todos los for con metodologías apply, sapply, func etc...
#################################################################################################
###############################################################################################
rm(list = ls()[!ls() %in% c()])
library(ncdf4)
library(lubridate)
library(xts)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dygraphs)
library(data.table)
library(raster)
#library(plotly)

MasterDir='/home/forever/Desktop/PROY/DATA/6mreal/'

variable = "prate"
################################# SE CREA UNA LISTA   ################################################

#trans <- read.csv("/home/forever/Desktop/PROY/lista_realtimebrck_ivan.csv")
trans = list.files("/home/forever/Desktop/PROY/DATA/6mreal/prate")
trans = as.list(trans)
TRANS <- as.vector(substr(trans,1,10))

##################SE ENCUENTRA EL NUMÉRO MENOR DE PRONOSTICOS HACIA ADELANTE EN TODOS LOS FORECAST #########3

vec = c()
for (k in TRANS) {
  a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6mreal/prate/",k,"_cortado.nc",sep=""))
  prate = ncvar_get(a2,"prate",start=c(1,2,1),count=c(1,1,-1))*6*3600 #OJO ACA! como esta tomando la grilla??
  
  tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
  tunitsp = ncatt_get(a2,"time","units")
  tustrp <- strsplit(tunitsp$value, " ")
  fini = unlist(tustrp)[3]
  hini = unlist(tustrp)[4]
  
  dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(12) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am
  serie <- data.frame(dh, prate)
  xtsserie = xts(serie[,-1], order.by=serie[,1])#3 convertir a xts
  xtsserie <- apply.daily(xtsserie,sum) #4 agregar a nivel diario
  df = data.frame(xtsserie) #6 convertir a df
  
  vec <- c(vec,max(dim(df)))
  nc_close(a2)
}
######1)  VALOR MINIMO DE PRONÓSTICOS EN UN ARCHIVO... TODOS DEBEN TENER EL MISMO LARGO
m = min(vec)
mm = max(vec)
largo = length(TRANS)

N_A = rep(0:largo, each=4, len=(largo))

N_A <- as.data.frame(N_A)
################################ CREACION DE MATRIZ MULTIDIMENSIONAL ###################################
######2) contar los archivos NN
NN = length(vec)
vecNC = vec+N_A

LM = max(vecNC) 

NAarriba=N_A




######3) crear vector de horas VH de largo LM
K2 = TRANS[1]
a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6mreal/prate/",K2,"_cortado.nc",sep=""))
tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
tunitsp = ncatt_get(a2,"time","units")
tustrp <- strsplit(tunitsp$value, " ")
fini = unlist(tustrp)[3]
hini = unlist(tustrp)[4]

lon = ncvar_get(a2,"lon")
lon <- c(rep(1,(length(lon)))*360-lon)*-1
lat = ncvar_get(a2,"lat")
lat <- rev(lat) #OJO QUE AQUI LO DI VUELTA!! No se por que motivo lo hice pero no olvidar !
# Indices 
laidx = seq(1, length(lat), 1)
loidx = seq(1, length(lon), 1)
#listapuntos = expand.grid(laidx,loidx)
nc_close(a2)

###esta linea la agrego solamente por que
### estoy u sando una grilla menor (3x3) a la completa (7x5)
laidx <- c(1,2,3)
loidx <- c(1,2,3)

VH = ymd(paste(fini), tz = "UTC") - days(1) + days(0:(LM-1))
VH = as.data.frame(VH)
colnames(VH) <- c("day")

BRCK = array(as.data.frame(VH, colnames = c("day")), dim=c(length(laidx),length(loidx))) #probar esta linea pé
for (lo in loidx) {
  for (la in laidx) {
    as.data.frame(BRCK[[la,lo]])
  }
}


for (archivo in TRANS) {
  #separar los numeros de archivo  para crear año_mes_diahora
  AAño = substr(archivo, 1, 4)
  MMes = substr(archivo, 5, 6)
  DDiahora = substr(archivo, 7, 10)
  
  a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6mreal/prate/",archivo,"_cortado.nc",sep=""))
  ########################
  ########################
  for (lo in loidx) {
    for (la in laidx) {
      
      tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
      tunitsp = ncatt_get(a2,"time","units")
      tustrp <- strsplit(tunitsp$value, " ")
      fini = unlist(tustrp)[3]
      hini = unlist(tustrp)[4]
      #dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(12) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am chilenas
      #OJO ACA !
      dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(6) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am chilenas
      
      prate = ncvar_get(a2,"prate",start=c(lo+2,la+1,1),count=c(1,1,-1))*6*3600 # transformacion unidades: 6 horas acum
      
      serie <- data.frame(dh, prate)
      xtsserie = xts(serie[,-1], order.by=serie[,1])#3 convertir a xts
      xtsserie <- apply.daily(xtsserie,sum) #4 agregar a nivel diario
      df = fortify(xtsserie) #fortify funciona la raja, pertenece a la libreria ggplot2 LLAMAR antes !
      #colnames(df) <- c(paste("Fecha",archivo,sep=""),paste("PRATE",archivo,sep=""))
      
      FCprate_diario = df$xtsserie
      
      # se crean los vectores con NA al principio y final correspondiente a cada archivo
      archivo <- as.integer(archivo)
      TRANS <- as.vector(TRANS)
      
      if (archivo==TRANS[1]) {
        NA_final = rep(NA, LM - length(FCprate_diario) ) 
        FCprate_diario_plusNA = c(FCprate_diario,NA_final)
        
      } else {
        
        
        NA_final = rep(NA, LM - NAarriba[which(TRANS==archivo)-1, ] -length(FCprate_diario)) 
        
        NA_principio = rep(NA, NAarriba[which(TRANS==archivo)-1, ])
        FCprate_diario_plusNA =  c(NA_principio,FCprate_diario,NA_final)
        
      }
      
      BRCK[[la,lo]] <- cbind.data.frame(BRCK[[la,lo]],FCprate_diario_plusNA)
      names(BRCK[[la,lo]])[length(names(BRCK[[la,lo]]))]<- paste(AAño,"_",MMes,"_",DDiahora,sep="") #nombrar aca a la ultima columna
    }                                                                    
  }
  nc_close(a2)
}

rm(list = ls()[!ls() %in% c("BRCK")]) #borro la basura
#load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/brck_operational.RData")
############## inputs:
nn = 12 # cantidad de ultimos datos a utilizar
trans = list.files("/home/forever/Desktop/PROY/DATA/6mreal/prate")
trans = as.list(trans)
TRANS <- as.vector(substr(trans,1,10))

dia_inicial = ymd(substr(TRANS[1],1,8))
dia_final = dia_inicial + days(14)
tol = 0.5 #mas grandes que este valor se cuentan como precipitación. preguntarle a la profe si corre
#################


geopuntos = c(1,2,3,4,5,6,7,8,9)
#test = BRCK[[2]]#
#largo = min(dim(test))
#dates = dia_inicial + days(0:(largo-1)) #creo el vector de fechas a recorrer
dates = seq(dia_inicial,dia_final, by = 'days')
dates <- as.list(dates)
#dates2 = write.csv(dates, "/home/forever/Desktop/PROY/dates2.csv")
#CREO UNA MATRIZ DE DATAFRAES 3x3 a rellenar
REALTIME = array(as.data.frame(0, colnames = c("day")), dim=c(3,3)) 
for (p in geopuntos) {
  REALTIME[[p]] = data.frame(matrix(vector(), 0, 2,
                                       dimnames=list(c(), c("fecha", "precipitacion"))),
                                stringsAsFactors=F)
}

for (k in geopuntos) {
  exdf = BRCK[[k]]
  for (k2 in dates) {
    #LINEA SIGUIENTE TIENE ERROR
    aux = dplyr::filter(exdf, ymd(`BRCK[[la, lo]]`)==ymd(paste(k2[[1]]))) #primero selecciono la fila que contiene el día que quiero analizar

    aux <- aux[ , ! apply( aux , 2 , function(x) all(is.na(x)) ) ] #borro las columnas con NA
    aux <- aux[tail(seq_along(aux),nn)] #dejo las n últimas columnas = n ultimas observaciones
    
    
    aux$`BRCK[[la, lo]]` <- NULL #borro la columna innecesaria de fechas (ya que solo está el dia de análisis)
    names(aux) <- NULL #borro los headers (en este caso eran los nombres de los archivos)
    aux <- c(aux)
    l = length(aux)
    lluvias = sum(aux > tol)
    
    if (l == 0) {
      ind = 0 
    } else {
      ind = lluvias/l
    }
    
    
    if (ind*nn >= nn/3.33 ) {
      aux <- aux[ aux > tol]
      aux <- unlist(aux,use.names=F)
      
      mm = mean(aux) 
      #acá debo agregar otro if, por que a veces mm queda < tol
      if (mm <= tol) {
        mm = 0
      }
      
    } else { 
      mm = 0 
    }
    
    registro = data.frame(k2, mm)
    REALTIME[[k]] <- rbind(REALTIME[[k]],registro) #se bindea como row el registro
  }
}

rm(list = ls()[!ls() %in% c("REALTIME")])
save.image("~/Desktop/PROY/APP/realtime_realtime.RData")


##################################
########## REAL TIME 
##############################################
rm(list = ls()[!ls() %in% c()])
library(sp)
library(lubridate)
library(dplyr)
library(MASS)
setwd("/home/forever/Desktop/PROY/APP")
load("realtime_realtime.RData")
load("statr_reforecast.RData")
forecast_list = list(STATR1,STATR2,STATR3,STATR4,STATR5,STATR6, 
                     STATR7,STATR8,STATR9,STATR10,STATR11,STATR12)

hest = read.csv("historico_estaciones.csv", stringsAsFactors = FALSE)
hest$DateTime = mdy(hest$DateTime)
est <- read.csv("est.csv")
pred <- read.csv("pred.csv")

nombres = names(hest)
nombres = gsub('\\ ', '.', nombres)
lista_estaciones = as.list(nombres[-1])

mes_actual = ymd_hms(Sys.time())
mes_actual <- month(mes_actual) #mes de ahora, arreglar en mediano plazo, la profe quiere coeficientes mas finos.

tol=0

est.sp <- SpatialPoints(list(est$lat,est$lon))
pred.sp <- SpatialPoints(list(pred$lat,pred$lon))
est$near <- apply(spDists(est.sp,pred.sp), 1, which.min)  


for (e in lista_estaciones) {
  e = as.character(e)
  e2 = gsub('\\.', ' ', e)
  nearest_station1 <- est %>%
    dplyr::filter(location == e2) %>%
    dplyr::select(near)
  nearest_station <- as.numeric(nearest_station1)
  
  data_realtime = REALTIME[[nearest_station]] %>% dplyr::select(mm)
  data_realtime2 <- unlist(data_realtime)
  
  registro_realtime <- hest %>%
    dplyr::select(DateTime,dplyr::contains(e)) %>%
    dplyr::mutate(m = month(DateTime)) %>%
    dplyr::filter(m == mes_actual) %>%
    dplyr::filter(get(e) > tol) %>%
    dplyr::select(contains(e))
  
  obsprecip_realtime <- sort(unlist(as.list(registro_realtime)))
  
  
  
  test_realtime <-  forecast_list[as.numeric(mes_actual)]
  modprecip_realtime <- sort(unlist(test_realtime[[1]][[nearest_station]]))
  
  #ajuste gamma con fitdistr de librería MASS
  
  param.observado_realtime <- fitdistr(obsprecip_realtime, "gamma", start=list(shape=1, scale=1), lower=0.1)
  param.reforecast_realtime <- fitdistr(modprecip_realtime, "gamma", start=list(shape=1, scale=1), lower = 0.5)
  
  shape.observado_realtime = param.observado_realtime$estimate["shape"]
  scale.observado_realtime = param.observado_realtime$estimate["scale"]
  
  shape.reforecast_realtime = param.reforecast_realtime$estimate["shape"]
  scale.reforecast_realtime = param.reforecast_realtime$estimate["scale"] 
  
  
  vrt = pgamma(data_realtime2,scale=scale.reforecast_realtime,shape=shape.reforecast_realtime) 
  
  realtime_corregido = qgamma(vrt,scale=scale.observado_realtime,shape=shape.observado_realtime) #PREDICCIÓN CORREGIDA :)
  as.data.frame(realtime_corregido)
  realtimedates = REALTIME[[1]]$k2
  
  if (e == lista_estaciones[1]) {
    dfrealtime <- data_frame(realtimedates,realtime_corregido)
    names(dfrealtime)[length(names(dfrealtime))]<-e2 
  } else {
    dfrealtime <- cbind(dfrealtime,realtime_corregido)
    names(dfrealtime)[length(names(dfrealtime))]<-e2 
  }
  
}

n <- names(dfrealtime)
dfrealtime <- as.data.frame(t(dfrealtime[,-1])) #trasponer todos menos la primera
#colnames(dfrealtime) <- n
colnames(dfrealtime) <- realtimedates
write.csv(as.data.frame(realtimedates),"raster.csv")
dfrealtime = cbind(dfrealtime,est$lat)
names(dfrealtime)[length(names(dfrealtime))]<-"lat"
dfrealtime = cbind(dfrealtime,est$lon)
names(dfrealtime)[length(names(dfrealtime))]<-"lon" 
dfrealtime = cbind(dfrealtime,est$alt) # agrego la altura
names(dfrealtime)[length(names(dfrealtime))]<-"alt" 

write.csv(dfrealtime, "dfrealtime.csv")
write.csv(dfrealtime, "/home/forever/Dropbox/nautilus/dfrealtime.csv")

library(rgdal)
library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
d <- read.csv("/home/forever/Desktop/PROY/APP/dfrealtime.csv")
W <-  readOGR(dsn = "/home/forever/Desktop/PROY/APP/shape/cuencas.shp", layer = "cuencas")
P <- SpatialPoints(d[,18:17], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#ojo acá que lat, lon estan en 18,17 por lo tanto si agrego mas estaciones eso va a cambiar :P
# pero ahora lo cambié a horizontal por tanto no cambia con agregar mas estaciones, va a depender de la cantidad de días.
P <- SpatialPointsDataFrame(P, d)
P@bbox <- W@bbox

grd              <- as.data.frame(spsample(P, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(grd) <- proj4string(P)

list_days = names(P@data)
list_days <- list_days[-1] #borro la primera
list_days <- list_days[-length(list_days)] #borro la ultima (alt)
list_days <- list_days[-length(list_days)] #borro la ultima (lon)
list_days <- list_days[-length(list_days)] #borro la ultima (alt)
list_days = as.list(noquote(list_days))


for (day in list_days) {
  dia_nombre = as.character(day)
  P.idw <- gstat::idw(formula = get(day) ~ 1, P, newdata=grd, idp=2.0)
  r       <- raster(P.idw)
  r.m     <- mask(r, W)
  writeRaster(r.m,paste("/home/forever/Dropbox/nautilus/",dia_nombre,".tif",sep=""),format = "GTiff", overwrite=T)
  writeRaster(r.m,paste("/home/forever/Desktop/PROY/APP/raster/",dia_nombre,".tif",sep=""),format = "GTiff",overwrite=T)
}

#lista a leer de los nombres de los raster
days_df = data.frame(list_days)
days_df <- as.data.frame(t(days_df[,-1]))
colnames(days_df) <- "raster"
write.csv(days_df, "/home/forever/Desktop/PROY/APP/days_df.csv")
write.csv(days_df, "/home/forever/Dropbox/nautilus/days_df.csv")




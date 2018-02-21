#####################################################################################################
############################ BRICK MAKER ############################################################
#.libPaths(new = "/home/forever/R/x86_64-pc-linux-gnu-library/3.4")

# Este código crea una matriz multidimensional a partir de archivos que entrega Catcher.R
# demora 4 minutos x año ( 4 min x 1.2 años) con metodología actual, 
# 64 megas por año para 7x5 puntos

# no olvidar que para subir velocidad debo: 
# paralelizar calculos y utilizar al menos 5 nucleos
# reemplazar todos los for con metodologías apply, sapply, func etc...

# error:
# 
#################################################################################################
###############################################################################################

library(ncdf4)
library(lubridate)
library(xts)
library(dplyr)
library(ggplot2)
library(tidyverse)

MasterDir='/home/forever/Desktop/PROY/DATA/6m/'

yearini = ymd(19940101)
yearfin = ymd(19951231)

variable = "prate"

####### nueva parte lista

url <- read.csv("/home/forever/Desktop/PROY/lista_reforecast_ivan.csv")
URL <- as.data.frame(url$lista)
URL2 <- as.vector(URL$`url$lista`)

################################# SE CREA UNA LISTA   ################################################

fechas = as.data.frame(seq(yearini,yearfin, by = '5 day')) #se genera el vector de fechas entre inicial y final
fechas <- fechas[rep(1:nrow(fechas),each=4),]  # se clona cada fila 4 veces... etc

dia = day(fechas) 
dia <- formatC(c(dia),flag=0,width=2)

mes = month(fechas)
mes <- formatC(c(mes),flag=0,width=2)

año = year(fechas)

horas = rep(c(00,06,12,18),times=(length(año)/4)) # se crea una lista de 00 06 12 18 repetitiva
horas <- formatC(c(horas),flag=0,width=2)

fechasDF2 = as.data.frame(fechas) %>%
  mutate(lista2 = paste(año,mes,dia,horas,sep="")) %>%
  select(lista2)
write.table(fechasDF2, file = "/home/forever/Desktop/PROY/listatrans.csv", row.names = FALSE)



## ACA SE LEE LA LISTA !!! ya esta actualizado
trans <- read.csv("/home/forever/Desktop/PROY/lista_reforecast_ivan.csv") #listatrans taba puesta

TRANS <- as.vector(trans$lista)
#TRANS <- 

##################SE ENCUENTRA EL NUMÉRO MENOR DE PRONOSTICOS HACIA ADELANTE EN TODOS LOS FORECAST #########3

vec = c()
for (k in TRANS) {
  a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6m/prate/",k,"_cortado.nc",sep=""))
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

################################ CREACION DE MATRIZ MULTIDIMENSIONAL ###################################

######2) contar los archivos NN
NN = length(vec)

#para saber el maximo largo del BRCK o DF
vecNC = vec + seq(0, NN-1, 1) #sumar un vector de 0 a n
LM = max(vecNC)

######3) crear vector de horas VH de largo LM
KK = TRANS[1]
a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6m/prate/",KK,"_cortado.nc",sep=""))
tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
tunitsp = ncatt_get(a2,"time","units")
tustrp <- strsplit(tunitsp$value, " ")
fini = unlist(tustrp)[3]
hini = unlist(tustrp)[4]


lon <- c(rep(1,(length(lon)))*360-lon)*-1
lat = ncvar_get(a2,"lat")
lat <- rev(lat)
# Indices 
laidx = seq(1, length(lat), 1)
loidx = seq(1, length(lon), 1)
#listapuntos = expand.grid(laidx,loidx)
nc_close(a2)

VH = ymd(paste(fini), tz = "UTC") - days(1) + days(0:(LM-1))
VH = as.data.frame(VH)
colnames(VH) <- c("day")
nc_close(a2)
BRCK = array(as.data.frame(VH, colnames = c("day")), dim=c(length(lat),length(lon))) #probar esta linea pé
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
  
  a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6m/prate/",archivo,"_cortado.nc",sep=""))
  ########################
  ########################
  for (lo in loidx) {
    for (la in laidx) {
      
      tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
      tunitsp = ncatt_get(a2,"time","units")
      tustrp <- strsplit(tunitsp$value, " ")
      fini = unlist(tustrp)[3]
      hini = unlist(tustrp)[4]
      dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(12) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am chilenas
      
      prate = ncvar_get(a2,"prate",start=c(lo,la,1),count=c(1,1,-1))*6*3600 # transformacion unidades: 6 horas acum
      
      serie <- data.frame(dh, prate)
      xtsserie = xts(serie[,-1], order.by=serie[,1])#3 convertir a xts
      xtsserie <- apply.daily(xtsserie,sum) #4 agregar a nivel diario
      df = fortify(xtsserie) #fortify funciona la raja, pertenece a la libreria ggplot2 LLAMAR antes !
      #colnames(df) <- c(paste("Fecha",archivo,sep=""),paste("PRATE",archivo,sep=""))
      
      FCprate_diario = df$xtsserie
      
      # se crean los vectores con NA al principio y final correspondiente a cada archivo
      archivo <- as.integer(archivo)
      TRANS <- as.vector(TRANS)
      
      #ACA TA EL ERRORCITO, problemas de dimension D: ?????? 
      if (archivo==TRANS[1]) {
        
        cant_NA = max(dim(VH))-length(FCprate_diario)
        NA_final = rep(NA, cant_NA) 
        FCprate_diario_plusNA = c(FCprate_diario,NA_final)#FALTA HACERLE EL APPEND DE NA
        
      } else {
        
        cant_NA = max(dim(VH))-length(FCprate_diario)
        cant_NA_final = which(TRANS == archivo) - 1
        NA_principio = rep(NA, cant_NA_final)
        NA_final = rep(NA,cant_NA - length(NA_principio))
        FCprate_diario_plusNA =  c(NA_principio,FCprate_diario,NA_final)
        
      }
      
      # ESPECIFICAMENTE ACA TA EL EMBROLLO POSOM, hay que agregar los NA. al parecer no hay embrollo xd
      BRCK[[la,lo]] <- cbind.data.frame(BRCK[[la,lo]],FCprate_diario_plusNA)
      names(BRCK[[la,lo]])[length(names(BRCK[[la,lo]]))]<- paste(AAño,"_",MMes,"_",DDiahora,sep="") #nombrar aca a la ultima columna
    }                                                                    
  }
  nc_close(a2)
}


#SOLO FALTA UNIFICAR LOS RESULTADOS EN UNA SOLA MATRIZ

tol = 0.5

for(i in 1:12){
  assign(paste("STAT",i,sep=""),array(list(NA), dim=c(length(lat),length(lon)))) #vectores vacios para cada mes 
  
  for (lo in loidx) {
    for (la in laidx) {
      assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), as.data.frame(do.call(rbind,BRCK[la,lo]))) #se toman los valores del ladrillo
      
      # se cortan solo los elementos que tengan el mes i-esimo (meses 01-09)
      if (i < 10){
        #corto las filas que contienen al mes ql
        assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), get(paste("STAT",i,"[",la,",",lo,"]",sep=""))[grep(paste("-0",i,"-",sep=""), get(paste("STAT",i,"[",la,",",lo,"]",sep=""))[,1]), ])
        #aca corto las columnas que contengan al mes que se esta viendo, ACA PONER EL MES SIGUIENTE PARA QUE CORTE PA 2 MESES
        assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), select(get(paste("STAT",i,"[",la,",",lo,"]",sep="")), dplyr::contains(paste("_0",i,"_",sep="")))) 
      }
      # se cortan solo los elementos que tengan el mes i-esimo (meses 10-12)
      else {
        assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), get(paste("STAT",i,"[",la,",",lo,"]",sep=""))[grep(paste("-",i,"-",sep=""), get(paste("STAT",i,"[",la,",",lo,"]",sep=""))[,1]), ])
        #aca corto las columnas que contengan al mes que se esta viendo
        assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), select(get(paste("STAT",i,"[",la,",",lo,"]",sep="")), dplyr::contains(paste("_",i,"_",sep=""))))
      }
      
      # se borra la columna 1. OJO EL SELECT ANTERIOR DEBERIA BORRAR LA PRIMERA
      #assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), get(paste("STAT",i,"[",la,",",lo,"]",sep=""))[,-1]) 
      #se unifican todos los datos en una sola columna prate
      assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), data.frame(prate = unlist(get(paste("STAT",i,"[",la,",",lo,"]",sep="")), use.names = FALSE))) #=TRUE??
      #se filtran los valores que sean mayores a la tolerancia tol
      assign(paste("STAT",i,"[",la,",",lo,"]",sep=""), filter(get(paste("STAT",i,"[",la,",",lo,"]",sep="")), prate>tol)) 
    }
  }
}

# ESTA PARTE FALTA GENERALIZARLA, DE  MOMENTO QUEDARÁ MANUAL, extremadamente larga (pero rapida)

STAT1[1,1]=`STAT1[1,1]`;STAT1[1,2]=`STAT1[1,2]`;STAT1[1,3]=`STAT1[1,3]`;STAT1[1,4]=`STAT1[1,4]`;STAT1[1,5]=`STAT1[1,5]`
STAT1[1,6]=`STAT1[1,6]`;STAT1[1,7]=`STAT1[1,7]`;STAT1[2,1]=`STAT1[2,1]`;STAT1[2,2]=`STAT1[2,2]`;STAT1[2,3]=`STAT1[2,3]`
STAT1[2,4]=`STAT1[2,4]`;STAT1[2,5]=`STAT1[2,5]`;STAT1[2,6]=`STAT1[2,6]`;STAT1[2,7]=`STAT1[2,7]`;STAT1[3,1]=`STAT1[3,1]`
STAT1[3,2]=`STAT1[3,2]`;STAT1[3,3]=`STAT1[3,3]`;STAT1[3,4]=`STAT1[3,4]`;STAT1[3,5]=`STAT1[3,5]`;STAT1[3,6]=`STAT1[3,6]`
STAT1[3,7]=`STAT1[3,7]`;STAT1[4,1]=`STAT1[4,1]`;STAT1[4,2]=`STAT1[4,2]`;STAT1[4,3]=`STAT1[4,3]`;STAT1[4,4]=`STAT1[4,4]`
STAT1[4,5]=`STAT1[4,5]`;STAT1[4,6]=`STAT1[4,6]`;STAT1[4,7]=`STAT1[4,7]`;STAT1[5,1]=`STAT1[5,1]`;STAT1[5,2]=`STAT1[5,2]`
STAT1[5,3]=`STAT1[5,3]`;STAT1[5,4]=`STAT1[5,4]`;STAT1[5,5]=`STAT1[5,5]`;STAT1[5,6]=`STAT1[5,6]`;STAT1[5,7]=`STAT1[5,7]`

STAT2[1,1]=`STAT2[1,1]`;STAT2[1,2]=`STAT2[1,2]`;STAT2[1,3]=`STAT2[1,3]`;STAT2[1,4]=`STAT2[1,4]`;STAT2[1,5]=`STAT2[1,5]`
STAT2[1,6]=`STAT2[1,6]`;STAT2[1,7]=`STAT2[1,7]`;STAT2[2,1]=`STAT2[2,1]`;STAT2[2,2]=`STAT2[2,2]`;STAT2[2,3]=`STAT2[2,3]`
STAT2[2,4]=`STAT2[2,4]`;STAT2[2,5]=`STAT2[2,5]`;STAT2[2,6]=`STAT2[2,6]`;STAT2[2,7]=`STAT2[2,7]`;STAT2[3,1]=`STAT2[3,1]`
STAT2[3,2]=`STAT2[3,2]`;STAT2[3,3]=`STAT2[3,3]`;STAT2[3,4]=`STAT2[3,4]`;STAT2[3,5]=`STAT2[3,5]`;STAT2[3,6]=`STAT2[3,6]`
STAT2[3,7]=`STAT2[3,7]`;STAT2[4,1]=`STAT2[4,1]`;STAT2[4,2]=`STAT2[4,2]`;STAT2[4,3]=`STAT2[4,3]`;STAT2[4,4]=`STAT2[4,4]`
STAT2[4,5]=`STAT2[4,5]`;STAT2[4,6]=`STAT2[4,6]`;STAT2[4,7]=`STAT2[4,7]`;STAT2[5,1]=`STAT2[5,1]`;STAT2[5,2]=`STAT2[5,2]`
STAT2[5,3]=`STAT2[5,3]`;STAT2[5,4]=`STAT2[5,4]`;STAT2[5,5]=`STAT2[5,5]`;STAT2[5,6]=`STAT2[5,6]`;STAT2[5,7]=`STAT2[5,7]`

STAT3[1,1]=`STAT3[1,1]`;STAT3[1,2]=`STAT3[1,2]`;STAT3[1,3]=`STAT3[1,3]`;STAT3[1,4]=`STAT3[1,4]`;STAT3[1,5]=`STAT3[1,5]`
STAT3[1,6]=`STAT3[1,6]`;STAT3[1,7]=`STAT3[1,7]`;STAT3[2,1]=`STAT3[2,1]`;STAT3[2,2]=`STAT3[2,2]`;STAT3[2,3]=`STAT3[2,3]`
STAT3[2,4]=`STAT3[2,4]`;STAT3[2,5]=`STAT3[2,5]`;STAT3[2,6]=`STAT3[2,6]`;STAT3[2,7]=`STAT3[2,7]`;STAT3[3,1]=`STAT3[3,1]`
STAT3[3,2]=`STAT3[3,2]`;STAT3[3,3]=`STAT3[3,3]`;STAT3[3,4]=`STAT3[3,4]`;STAT3[3,5]=`STAT3[3,5]`;STAT3[3,6]=`STAT3[3,6]`
STAT3[3,7]=`STAT3[3,7]`;STAT3[4,1]=`STAT3[4,1]`;STAT3[4,2]=`STAT3[4,2]`;STAT3[4,3]=`STAT3[4,3]`;STAT3[4,4]=`STAT3[4,4]`
STAT3[4,5]=`STAT3[4,5]`;STAT3[4,6]=`STAT3[4,6]`;STAT3[4,7]=`STAT3[4,7]`;STAT3[5,1]=`STAT3[5,1]`;STAT3[5,2]=`STAT3[5,2]`
STAT3[5,3]=`STAT3[5,3]`;STAT3[5,4]=`STAT3[5,4]`;STAT3[5,5]=`STAT3[5,5]`;STAT3[5,6]=`STAT3[5,6]`;STAT3[5,7]=`STAT3[5,7]`

STAT4[1,1]=`STAT4[1,1]`;STAT4[1,2]=`STAT4[1,2]`;STAT4[1,3]=`STAT4[1,3]`;STAT4[1,4]=`STAT4[1,4]`;STAT4[1,5]=`STAT4[1,5]`
STAT4[1,6]=`STAT4[1,6]`;STAT4[1,7]=`STAT4[1,7]`;STAT4[2,1]=`STAT4[2,1]`;STAT4[2,2]=`STAT4[2,2]`;STAT4[2,3]=`STAT4[2,3]`
STAT4[2,4]=`STAT4[2,4]`;STAT4[2,5]=`STAT4[2,5]`;STAT4[2,6]=`STAT4[2,6]`;STAT4[2,7]=`STAT4[2,7]`;STAT4[3,1]=`STAT4[3,1]`
STAT4[3,2]=`STAT4[3,2]`;STAT4[3,3]=`STAT4[3,3]`;STAT4[3,4]=`STAT4[3,4]`;STAT4[3,5]=`STAT4[3,5]`;STAT4[3,6]=`STAT4[3,6]`
STAT4[3,7]=`STAT4[3,7]`;STAT4[4,1]=`STAT4[4,1]`;STAT4[4,2]=`STAT4[4,2]`;STAT4[4,3]=`STAT4[4,3]`;STAT4[4,4]=`STAT4[4,4]`
STAT4[4,5]=`STAT4[4,5]`;STAT4[4,6]=`STAT4[4,6]`;STAT4[4,7]=`STAT4[4,7]`;STAT4[5,1]=`STAT4[5,1]`;STAT4[5,2]=`STAT4[5,2]`
STAT4[5,3]=`STAT4[5,3]`;STAT4[5,4]=`STAT4[5,4]`;STAT4[5,5]=`STAT4[5,5]`;STAT4[5,6]=`STAT4[5,6]`;STAT4[5,7]=`STAT4[5,7]`

STAT5[1,1]=`STAT5[1,1]`;STAT5[1,2]=`STAT5[1,2]`;STAT5[1,3]=`STAT5[1,3]`;STAT5[1,4]=`STAT5[1,4]`;STAT5[1,5]=`STAT5[1,5]`
STAT5[1,6]=`STAT5[1,6]`;STAT5[1,7]=`STAT5[1,7]`;STAT5[2,1]=`STAT5[2,1]`;STAT5[2,2]=`STAT5[2,2]`;STAT5[2,3]=`STAT5[2,3]`
STAT5[2,4]=`STAT5[2,4]`;STAT5[2,5]=`STAT5[2,5]`;STAT5[2,6]=`STAT5[2,6]`;STAT5[2,7]=`STAT5[2,7]`;STAT5[3,1]=`STAT5[3,1]`
STAT5[3,2]=`STAT5[3,2]`;STAT5[3,3]=`STAT5[3,3]`;STAT5[3,4]=`STAT5[3,4]`;STAT5[3,5]=`STAT5[3,5]`;STAT5[3,6]=`STAT5[3,6]`
STAT5[3,7]=`STAT5[3,7]`;STAT5[4,1]=`STAT5[4,1]`;STAT5[4,2]=`STAT5[4,2]`;STAT5[4,3]=`STAT5[4,3]`;STAT5[4,4]=`STAT5[4,4]`
STAT5[4,5]=`STAT5[4,5]`;STAT5[4,6]=`STAT5[4,6]`;STAT5[4,7]=`STAT5[4,7]`;STAT5[5,1]=`STAT5[5,1]`;STAT5[5,2]=`STAT5[5,2]`
STAT5[5,3]=`STAT5[5,3]`;STAT5[5,4]=`STAT5[5,4]`;STAT5[5,5]=`STAT5[5,5]`;STAT5[5,6]=`STAT5[5,6]`;STAT5[5,7]=`STAT5[5,7]`

STAT6[1,1]=`STAT6[1,1]`;STAT6[1,2]=`STAT6[1,2]`;STAT6[1,3]=`STAT6[1,3]`;STAT6[1,4]=`STAT6[1,4]`;STAT6[1,5]=`STAT6[1,5]`
STAT6[1,6]=`STAT6[1,6]`;STAT6[1,7]=`STAT6[1,7]`;STAT6[2,1]=`STAT6[2,1]`;STAT6[2,2]=`STAT6[2,2]`;STAT6[2,3]=`STAT6[2,3]`
STAT6[2,4]=`STAT6[2,4]`;STAT6[2,5]=`STAT6[2,5]`;STAT6[2,6]=`STAT6[2,6]`;STAT6[2,7]=`STAT6[2,7]`;STAT6[3,1]=`STAT6[3,1]`
STAT6[3,2]=`STAT6[3,2]`;STAT6[3,3]=`STAT6[3,3]`;STAT6[3,4]=`STAT6[3,4]`;STAT6[3,5]=`STAT6[3,5]`;STAT6[3,6]=`STAT6[3,6]`
STAT6[3,7]=`STAT6[3,7]`;STAT6[4,1]=`STAT6[4,1]`;STAT6[4,2]=`STAT6[4,2]`;STAT6[4,3]=`STAT6[4,3]`;STAT6[4,4]=`STAT6[4,4]`
STAT6[4,5]=`STAT6[4,5]`;STAT6[4,6]=`STAT6[4,6]`;STAT6[4,7]=`STAT6[4,7]`;STAT6[5,1]=`STAT6[5,1]`;STAT6[5,2]=`STAT6[5,2]`
STAT6[5,3]=`STAT6[5,3]`;STAT6[5,4]=`STAT6[5,4]`;STAT6[5,5]=`STAT6[5,5]`;STAT6[5,6]=`STAT6[5,6]`;STAT6[5,7]=`STAT6[5,7]`

STAT7[1,1]=`STAT7[1,1]`;STAT7[1,2]=`STAT7[1,2]`;STAT7[1,3]=`STAT7[1,3]`;STAT7[1,4]=`STAT7[1,4]`;STAT7[1,5]=`STAT7[1,5]`
STAT7[1,6]=`STAT7[1,6]`;STAT7[1,7]=`STAT7[1,7]`;STAT7[2,1]=`STAT7[2,1]`;STAT7[2,2]=`STAT7[2,2]`;STAT7[2,3]=`STAT7[2,3]`
STAT7[2,4]=`STAT7[2,4]`;STAT7[2,5]=`STAT7[2,5]`;STAT7[2,6]=`STAT7[2,6]`;STAT7[2,7]=`STAT7[2,7]`;STAT7[3,1]=`STAT7[3,1]`
STAT7[3,2]=`STAT7[3,2]`;STAT7[3,3]=`STAT7[3,3]`;STAT7[3,4]=`STAT7[3,4]`;STAT7[3,5]=`STAT7[3,5]`;STAT7[3,6]=`STAT7[3,6]`
STAT7[3,7]=`STAT7[3,7]`;STAT7[4,1]=`STAT7[4,1]`;STAT7[4,2]=`STAT7[4,2]`;STAT7[4,3]=`STAT7[4,3]`;STAT7[4,4]=`STAT7[4,4]`
STAT7[4,5]=`STAT7[4,5]`;STAT7[4,6]=`STAT7[4,6]`;STAT7[4,7]=`STAT7[4,7]`;STAT7[5,1]=`STAT7[5,1]`;STAT7[5,2]=`STAT7[5,2]`
STAT7[5,3]=`STAT7[5,3]`;STAT7[5,4]=`STAT7[5,4]`;STAT7[5,5]=`STAT7[5,5]`;STAT7[5,6]=`STAT7[5,6]`;STAT7[5,7]=`STAT7[5,7]`

STAT8[1,1]=`STAT8[1,1]`;STAT8[1,2]=`STAT8[1,2]`;STAT8[1,3]=`STAT8[1,3]`;STAT8[1,4]=`STAT8[1,4]`;STAT8[1,5]=`STAT8[1,5]`
STAT8[1,6]=`STAT8[1,6]`;STAT8[1,7]=`STAT8[1,7]`;STAT8[2,1]=`STAT8[2,1]`;STAT8[2,2]=`STAT8[2,2]`;STAT8[2,3]=`STAT8[2,3]`
STAT8[2,4]=`STAT8[2,4]`;STAT8[2,5]=`STAT8[2,5]`;STAT8[2,6]=`STAT8[2,6]`;STAT8[2,7]=`STAT8[2,7]`;STAT8[3,1]=`STAT8[3,1]`
STAT8[3,2]=`STAT8[3,2]`;STAT8[3,3]=`STAT8[3,3]`;STAT8[3,4]=`STAT8[3,4]`;STAT8[3,5]=`STAT8[3,5]`;STAT8[3,6]=`STAT8[3,6]`
STAT8[3,7]=`STAT8[3,7]`;STAT8[4,1]=`STAT8[4,1]`;STAT8[4,2]=`STAT8[4,2]`;STAT8[4,3]=`STAT8[4,3]`;STAT8[4,4]=`STAT8[4,4]`
STAT8[4,5]=`STAT8[4,5]`;STAT8[4,6]=`STAT8[4,6]`;STAT8[4,7]=`STAT8[4,7]`;STAT8[5,1]=`STAT8[5,1]`;STAT8[5,2]=`STAT8[5,2]`
STAT8[5,3]=`STAT8[5,3]`;STAT8[5,4]=`STAT8[5,4]`;STAT8[5,5]=`STAT8[5,5]`;STAT8[5,6]=`STAT8[5,6]`;STAT8[5,7]=`STAT8[5,7]`

STAT9[1,1]=`STAT9[1,1]`;STAT9[1,2]=`STAT9[1,2]`;STAT9[1,3]=`STAT9[1,3]`;STAT9[1,4]=`STAT9[1,4]`;STAT9[1,5]=`STAT9[1,5]`
STAT9[1,6]=`STAT9[1,6]`;STAT9[1,7]=`STAT9[1,7]`;STAT9[2,1]=`STAT9[2,1]`;STAT9[2,2]=`STAT9[2,2]`;STAT9[2,3]=`STAT9[2,3]`
STAT9[2,4]=`STAT9[2,4]`;STAT9[2,5]=`STAT9[2,5]`;STAT9[2,6]=`STAT9[2,6]`;STAT9[2,7]=`STAT9[2,7]`;STAT9[3,1]=`STAT9[3,1]`
STAT9[3,2]=`STAT9[3,2]`;STAT9[3,3]=`STAT9[3,3]`;STAT9[3,4]=`STAT9[3,4]`;STAT9[3,5]=`STAT9[3,5]`;STAT9[3,6]=`STAT9[3,6]`
STAT9[3,7]=`STAT9[3,7]`;STAT9[4,1]=`STAT9[4,1]`;STAT9[4,2]=`STAT9[4,2]`;STAT9[4,3]=`STAT9[4,3]`;STAT9[4,4]=`STAT9[4,4]`
STAT9[4,5]=`STAT9[4,5]`;STAT9[4,6]=`STAT9[4,6]`;STAT9[4,7]=`STAT9[4,7]`;STAT9[5,1]=`STAT9[5,1]`;STAT9[5,2]=`STAT9[5,2]`
STAT9[5,3]=`STAT9[5,3]`;STAT9[5,4]=`STAT9[5,4]`;STAT9[5,5]=`STAT9[5,5]`;STAT9[5,6]=`STAT9[5,6]`;STAT9[5,7]=`STAT9[5,7]`

STAT10[1,1]=`STAT10[1,1]`;STAT10[1,2]=`STAT10[1,2]`;STAT10[1,3]=`STAT10[1,3]`;STAT10[1,4]=`STAT10[1,4]`;STAT10[1,5]=`STAT10[1,5]`
STAT10[1,6]=`STAT10[1,6]`;STAT10[1,7]=`STAT10[1,7]`;STAT10[2,1]=`STAT10[2,1]`;STAT10[2,2]=`STAT10[2,2]`;STAT10[2,3]=`STAT10[2,3]`
STAT10[2,4]=`STAT10[2,4]`;STAT10[2,5]=`STAT10[2,5]`;STAT10[2,6]=`STAT10[2,6]`;STAT10[2,7]=`STAT10[2,7]`;STAT10[3,1]=`STAT10[3,1]`
STAT10[3,2]=`STAT10[3,2]`;STAT10[3,3]=`STAT10[3,3]`;STAT10[3,4]=`STAT10[3,4]`;STAT10[3,5]=`STAT10[3,5]`;STAT10[3,6]=`STAT10[3,6]`
STAT10[3,7]=`STAT10[3,7]`;STAT10[4,1]=`STAT10[4,1]`;STAT10[4,2]=`STAT10[4,2]`;STAT10[4,3]=`STAT10[4,3]`;STAT10[4,4]=`STAT10[4,4]`
STAT10[4,5]=`STAT10[4,5]`;STAT10[4,6]=`STAT10[4,6]`;STAT10[4,7]=`STAT10[4,7]`;STAT10[5,1]=`STAT10[5,1]`;STAT10[5,2]=`STAT10[5,2]`
STAT10[5,3]=`STAT10[5,3]`;STAT10[5,4]=`STAT10[5,4]`;STAT10[5,5]=`STAT10[5,5]`;STAT10[5,6]=`STAT10[5,6]`;STAT10[5,7]=`STAT10[5,7]`

STAT11[1,1]=`STAT11[1,1]`;STAT11[1,2]=`STAT11[1,2]`;STAT11[1,3]=`STAT11[1,3]`;STAT11[1,4]=`STAT11[1,4]`;STAT11[1,5]=`STAT11[1,5]`
STAT11[1,6]=`STAT11[1,6]`;STAT11[1,7]=`STAT11[1,7]`;STAT11[2,1]=`STAT11[2,1]`;STAT11[2,2]=`STAT11[2,2]`;STAT11[2,3]=`STAT11[2,3]`
STAT11[2,4]=`STAT11[2,4]`;STAT11[2,5]=`STAT11[2,5]`;STAT11[2,6]=`STAT11[2,6]`;STAT11[2,7]=`STAT11[2,7]`;STAT11[3,1]=`STAT11[3,1]`
STAT11[3,2]=`STAT11[3,2]`;STAT11[3,3]=`STAT11[3,3]`;STAT11[3,4]=`STAT11[3,4]`;STAT11[3,5]=`STAT11[3,5]`;STAT11[3,6]=`STAT11[3,6]`
STAT11[3,7]=`STAT11[3,7]`;STAT11[4,1]=`STAT11[4,1]`;STAT11[4,2]=`STAT11[4,2]`;STAT11[4,3]=`STAT11[4,3]`;STAT11[4,4]=`STAT11[4,4]`
STAT11[4,5]=`STAT11[4,5]`;STAT11[4,6]=`STAT11[4,6]`;STAT11[4,7]=`STAT11[4,7]`;STAT11[5,1]=`STAT11[5,1]`;STAT11[5,2]=`STAT11[5,2]`
STAT11[5,3]=`STAT11[5,3]`;STAT11[5,4]=`STAT11[5,4]`;STAT11[5,5]=`STAT11[5,5]`;STAT11[5,6]=`STAT11[5,6]`;STAT11[5,7]=`STAT11[5,7]`

STAT12[1,1]=`STAT12[1,1]`;STAT12[1,2]=`STAT12[1,2]`;STAT12[1,3]=`STAT12[1,3]`;STAT12[1,4]=`STAT12[1,4]`;STAT12[1,5]=`STAT12[1,5]`
STAT12[1,6]=`STAT12[1,6]`;STAT12[1,7]=`STAT12[1,7]`;STAT12[2,1]=`STAT12[2,1]`;STAT12[2,2]=`STAT12[2,2]`;STAT12[2,3]=`STAT12[2,3]`
STAT12[2,4]=`STAT12[2,4]`;STAT12[2,5]=`STAT12[2,5]`;STAT12[2,6]=`STAT12[2,6]`;STAT12[2,7]=`STAT12[2,7]`;STAT12[3,1]=`STAT12[3,1]`
STAT12[3,2]=`STAT12[3,2]`;STAT12[3,3]=`STAT12[3,3]`;STAT12[3,4]=`STAT12[3,4]`;STAT12[3,5]=`STAT12[3,5]`;STAT12[3,6]=`STAT12[3,6]`
STAT12[3,7]=`STAT12[3,7]`;STAT12[4,1]=`STAT12[4,1]`;STAT12[4,2]=`STAT12[4,2]`;STAT12[4,3]=`STAT12[4,3]`;STAT12[4,4]=`STAT12[4,4]`
STAT12[4,5]=`STAT12[4,5]`;STAT12[4,6]=`STAT12[4,6]`;STAT12[4,7]=`STAT12[4,7]`;STAT12[5,1]=`STAT12[5,1]`;STAT12[5,2]=`STAT12[5,2]`
STAT12[5,3]=`STAT12[5,3]`;STAT12[5,4]=`STAT12[5,4]`;STAT12[5,5]=`STAT12[5,5]`;STAT12[5,6]=`STAT12[5,6]`;STAT12[5,7]=`STAT12[5,7]`

# ... recordatorio viejo con sueño 5 am...
#STAT11 <- as.data.frame(do.call(rbind, BRCK[1,1]))

#selecciono ENERO para el punto 11
#STAT11_ENERO = STAT11[grep("-01-", STAT11$`BRCK[[la, lo]]`), ]
#borrar columna 1
#STAT11_ENERO = STAT11_ENERO[,-1]

#unificar todos los datos
#STAT11_ENERO <- data.frame(prate = unlist(STAT11_ENERO, use.names = FALSE))
#borrar filas con NAS y menores a una cierta tolerancia
#tol = 0.01
#STAT11_ENERO_FILT = filter(STAT11_ENERO, prate > tol)


##intento de generalizar todo ### MALO

#rm(a2,tunitsp,tustrp,fini,hini,dh,lat,lon,laidx,loidx,la,lo,prate) # remover basura para evitar bugs

############################################################ ANEXO: GRAFICA.

################################## EJEMPLO DE AJUSTE
# RECORDATORIO, EL CORTE QUEDÓ ASÍ :
#lat -35.43293 -34.48805 -33.54318 -32.59830 -31.65342
#lon -73.910 -72.972 -72.034 -71.096 -70.158 -69.220 -68.282
# Y CADA DF MENSUAL ESTA ASI:
#STAT[lat,lon]
#> lon
#[1] -73.910 -72.972 -72.034 -71.096 -70.158 -69.220 -68.282
#> lat
#[1] -35.43293 -34.48805 -33.54318 -32.59830 -31.65342
tol = 0.1
#ESTACION QUINTA NORMAL =)
QN = read.csv("/home/forever/Desktop/PROY/historico_quintanormal.csv")
QN <- as.data.frame(filter(QN, mm > tol))
Obs <- QN$mm

#selección de estación mas cercana. falta generalizar esto. Distancia plana? euclideana? que pasa con las alturas??
test = STAT7[3,4] #este es el punto mas cercano a quinta normal
test <- sort(unlist(test))

#testdf <- as.data.frame(STAT7[3,4])
#Obs = testdf %>% 
#  filter(c.2.74320004955371..2.9159998477553..0.280799996835412..13.4136002190644.. < tol)
#
#Obs <- sort(unlist(as.list(Obs)))

  


# Ajuste gamma a forecast
library(EnvStats)
param.gamma = egamma(test, method = "mle")
scale.gamma = param.gamma$parameters[2]
shape.gamma = param.gamma$parameters[1]

pron.gamma = rgamma(n=10000,scale=scale.gamma,shape=shape.gamma)

# Ajuste gamma a observaciones
obs.gammafit = egamma(Obs, method = "mle")
obs.scale.gamma = obs.gammafit$parameters[2]
obs.shape.gamma = obs.gammafit$parameters[1]

obs.gamma = rgamma(n=10000, scale=obs.scale.gamma, shape=obs.shape.gamma)

#GRAFICO
group <- c(rep("Estación Quinta Normal", length(obs.gamma)), rep("Pronosticado", length(pron.gamma)))
dat <- data.frame(KSD = c(obs.gamma,pron.gamma), group = group)


ggplot(dat, aes(x = KSD, group = group, color = group))+
  stat_ecdf(size=0.5) +
  theme_bw(base_size = 14) +
  theme(legend.position ="top") +
  xlab("Precipitación") +
  ylab("F(x)") +
  ggtitle("AJUSTE GAMMA") +
  theme(legend.title=element_blank())

#ajuste gumbel...
#parametros.gumbel2 = eevd(test, method = "mle")
#parametros.gumbel2.1 = parametros.gumbel2$parameters
#x.gumbel = rgumbel

#corrección

cdf1 <- ecdf(obs.gamma)
cdf2 <- ecdf(pron.gamma)
#otra forma de  graficar con lattice
#library(lattice)
#library(latticeExtra)
#vals <- data.frame(r1=obs.gamma,
#                   r2=pron.gamma)
#ecdfplot(~ r1 + r2, data=vals, auto.key=list(space='right'))

        



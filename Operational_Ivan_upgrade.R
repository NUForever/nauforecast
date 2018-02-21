#####################################################################################################
############################ BRICK MAKER ############################################################
#.libPaths(new = "/home/forever/R/x86_64-pc-linux-gnu-library/3.4")

# Este código crea una matriz multidimensional a partir de archivos que entrega Catcher.R
# reemplazar todos los for con metodologías apply, sapply, func etc...
#################################################################################################
###############################################################################################

library(ncdf4)
library(lubridate)
library(xts)
library(dplyr)
library(ggplot2)
library(tidyverse)


MasterDir='/home/forever/Desktop/PROY/DATA/6mope/'
#yearini = ymd(19940101)
#yearfin = ymd(19951231)
variable = "prate"
################################# SE CREA UNA LISTA   ################################################


## ACA SE LEE LA LISTA !!!
trans <- read.csv("/home/forever/Desktop/PROY/lista_operational_ivan.csv")
#trans <- read.csv("/home/forever/Desktop/PROY/lista_operational_ivan.csv") #esta estaba puesta
TRANS <- as.vector(trans$lista)

##################SE ENCUENTRA EL NUMÉRO MENOR DE PRONOSTICOS HACIA ADELANTE EN TODOS LOS FORECAST #########3

vec = c()
for (k in TRANS) {
  a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6mope/prate/",k,"_cortado.nc",sep=""))
  prate = ncvar_get(a2,"prate",start=c(1,2,1),count=c(1,1,-1))*6*3600 #OJO ACA! como esta tomando la grilla??
  
  tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
  tunitsp = ncatt_get(a2,"time","units")
  tustrp <- strsplit(tunitsp$value, " ")
  fini = unlist(tustrp)[3]
  hini = unlist(tustrp)[4]
  
  dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(18) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am
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

###LINEA NUEVA, cantidad de NAs que van SOBRE los vectores :)
lina = read.csv("/home/forever/Desktop/PROY/lista_operational_lina.csv")
#ivan = read.csv("/home/forever/Desktop/PROY/lista_operational_ivan.csv")
largo_lina = max(dim(lina))
#miss = (anti_join(lina,ivan))

N_A = rep(0:largo_lina, each=4, len=(largo_lina+3))
N_A <- as.data.frame(N_A)
N_A <- as.data.frame(N_A[-c(1, 2, 3, 
                            285,585, #2011 miss
                            1344,1478,1526,1541,1561,1562,1574,1979,1981,1987, #2012
                            2698,2705,3107,3108,3109,3110,3181,3182,3233,3234,3235,3236,3237,3239,3240,3241,3335, #2013
                            5365, #2014
                            6822, #2015
                            7276,7615,7659,7687,8389,8391), ]) #2016 #es index +3 pq hay 3 numeros de mas

################################ CREACION DE MATRIZ MULTIDIMENSIONAL ###################################
######2) contar los archivos NN
NN = length(vec)
vecNC = vec+N_A

LM = max(vecNC) 

NAarriba=N_A

######3) crear vector de horas VH de largo LM
KK = TRANS[1]
a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6mope/prate/",KK,"_cortado.nc",sep=""))
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
  
  a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6mope/prate/",archivo,"_cortado.nc",sep=""))
  ########################
  ########################
  for (lo in loidx) {
    for (la in laidx) {
      
      tp = ncvar_get(a2,"time")#1 obtener fecha inicial desde el ncdf
      tunitsp = ncatt_get(a2,"time","units")
      tustrp <- strsplit(tunitsp$value, " ")
      fini = unlist(tustrp)[3]
      hini = unlist(tustrp)[4]
      dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(18) + 6*hours(0:(length(tp)-1))#conversion a "chile" : las 00 están a las 8am chilenas
      
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
      
      # ESPECIFICAMENTE ACA TA EL EMBROLLO POSOM, hay que agregar los NA. al parecer no hay embrollo xd
      BRCK[[la,lo]] <- cbind.data.frame(BRCK[[la,lo]],FCprate_diario_plusNA)
      names(BRCK[[la,lo]])[length(names(BRCK[[la,lo]]))]<- paste(AAño,"_",MMes,"_",DDiahora,sep="") #nombrar aca a la ultima columna
    }                                                                    
  }
  nc_close(a2)
}

rm(list = ls()[!ls() %in% c("BRCK")]) #borro la basura
#load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/brck_operational.RData")
############## inputs:
n = 12 # cantidad de ultimos datos a utilizar
dia_inicial = ymd(20110331)
dia_final = ymd(20161231)
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
OPERATIONAL = array(as.data.frame(0, colnames = c("day")), dim=c(3,3)) 
for (p in geopuntos) {
    OPERATIONAL[[p]] = data.frame(matrix(vector(), 0, 2,
                                         dimnames=list(c(), c("fecha", "precipitacion"))),
                                  stringsAsFactors=F)
}

for (k in geopuntos) {
  exdf = BRCK[[k]]
  for (k2 in dates) {
    #LINEA SIGUIENTE TIENE ERROR
    aux = dplyr::filter(exdf, ymd(`BRCK[[la, lo]]`)==ymd(paste(k2[[1]]))) #primero selecciono la fila que contiene
    #aux = dplyr::filter(exdf, ymd(`BRCK[[la, lo]]`)==as.POSIXct(paste(k2[[1]],"UTC",sep=" "))) #primero selecciono la fila que contiene al día que quiero analizar
    aux <- aux[ , ! apply( aux , 2 , function(x) all(is.na(x)) ) ] #borro las columnas con NA
    aux <- aux[tail(seq_along(aux),n)] #dejo las n últimas columnas = n ultimas observaciones
    
    
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
    
    
    if (ind*n >= n/5 ) {
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
    OPERATIONAL[[k]] <- rbind(OPERATIONAL[[k]],registro) #se bindea como row el registro
  }
}

rm(list = ls()[!ls() %in% c("OPERATIONAL")])

##################################
##################################
# ejemplo de gráfica de xts
library(dygraphs)
library(xts)
library(data.table)
## PASOS PARA CREAR XTS (no olvidar... )
#1) se crea un df
load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/operational_operational_12days.RData")
dfoperational = OPERATIONAL[[2,2]]
# DEBO CORREGIR QM
dia_inicial = ymd(20110331)
dia_final = ymd(20161231)

#2) se hace esta ordenacion y listo
xtsoperational <- xts(dfoperational[,-1], order.by=dfoperational[,1])

dygraph(xtsoperational, main = "Precipitación Pronosticada") %>%
  dySeries(stepPlot = T, fillGraph = T, label = "mm") %>%
  dyLegend(width = 620) %>%
  dyRangeSelector(height = 40)


##evaluación

hest = read.csv("/home/forever/Desktop/PROY/historico_estaciones.csv", stringsAsFactors = FALSE)
hest$DateTime = mdy(hest$DateTime)
e2 = "QUINTA NORMAL SANTIAGO"
tol = 0.5
e = gsub('\\ ', '.', e2) 
######### primero obtengo el registro de la estación de entrada, para el mes que se busca.
registro = hest %>%
  dplyr::select(DateTime,dplyr::contains(e)) %>%
  dplyr::filter(DateTime >= dia_inicial & DateTime <= dia_final)

names(registro)[2] <- "mm"
#setDT(registro)[mm<tol,mm:=0] #reemplazo valores menores a tol por cero
#########
#convierto a xts para comparar
registro <- as.data.frame(registro)
xtsobservado <- xts(registro[,-1], order.by=registro[,1])

comp <- cbind(xtsoperational,xtsobservado)

dygraph(comp, main = "Comparación Quinta Normal, JULIO, tol  = 0.5 mm")%>%
  dySeries("..1",stepPlot = T, fillGraph = T, label = "[mm] Operational") %>%
  dySeries("..2",stepPlot = T, fillGraph = T, label = "[mm] Observado") %>%
  dyLegend(width = 620) %>%
  dyRangeSelector(height = 40)










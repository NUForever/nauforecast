#REFORECAST
# libPaths(new = "/home/forever/R/x86_64-pc-linux-gnu-library/3.4")
##### Este código crea una matriz multidimensional a partir de archivos que entrega Catcher.R

library(ncdf4)
library(lubridate)
library(xts)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sp)

#####Se define una dirección maestra (GENERALIZAR)
MasterDir='/home/forever/Desktop/PROY/DATA/6m/'
#####Se elige la variable a utilizar, en este caso es el rate de precipitación (prate)
variable = "prate"
#####Acá se lee la lista de archivos. Al parecer esta lista tiene 1 archivo de mas, ya que este no existe en el servidor.
trans <- read.csv("/home/forever/Desktop/PROY/lista_reforecast_ivan.csv")
#####transformo la lista a vector, para que el ciclo for pueda recorrerla.
TRANS <- as.vector(trans$lista)
#####A continuación se encuentra el menor número de pronósticos hacia adelante, ya que este va a ser el valor
#####máximo de largo que tendrán todos los pronósticos.

f1 = ymd_h(TRANS[1])
f2 = ymd_h(TRANS[length(TRANS)])
intervalo = f1 %--% f2
LM = round(intervalo/ddays(1)) + 500 # le sumo 500 para que no se quede corto el VH(corre para reforecast)
VH = ymd(f1, tz = "UTC") - days(1) + days(0:(LM-1))
VH = as.data.frame(VH)
colnames(VH) <- c("day")



#####Se crea un vector vacío, donde se van a rellenar los largos de cada vector

vec_NA = c()
for (k in TRANS) {
  
  k2 = substr(k, 1, 8) #año mes dia
  hora_archivo = as.integer(substr(k, 9,10))
  file_idx = which(VH$day==ymd(k2))
  
  if (hora_archivo==18) {
    cantidad_NA = file_idx -1
    vec_NA <- c(vec_NA,cantidad_NA)
  } else {
    cantidad_NA = file_idx-2
    vec_NA <- c(vec_NA,cantidad_NA)
  }
  
}

# Se extrae la información relevante del primer archivo .nc
KK = TRANS[1]
a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6m/prate/",KK,"_cortado.nc",sep=""))
# Se extrae el vector de longitudes que trae el archivo
lon = ncvar_get(a2,"lon")
# pero viene de la siguiente manera:
# 286.090 287.028 287.966 288.904 289.842 290.780 291.718
# por lo que se aplicará la siguiente conversión para que queden de la forma habitual:
lon <- c(rep(1,(length(lon)))*360-lon)*-1
#-73.910 -72.972 -72.034 -71.096 -70.158 -69.220 -68.282

# También se extrae el vector de latitudes, que viene de forma habitual:
lat = ncvar_get(a2,"lat")
# -31.65342 -32.59830 -33.54318 -34.48805 -35.43293

# Revierto el orden de las latitudes. NO RECUERDO EL POR QUE 
# CREO QUE NO AFECTA EN NADA, PERO NO OLVIDAR
lat <- rev(lat)
# -35.43293 -34.48805 -33.54318 -32.59830 -31.65342

# Se crea un vector de índices que luego recorreremos 
laidx = seq(1, length(lat), 1)
# 1 2 3 4 5
loidx = seq(1, length(lon), 1)
# 1 2 3 4 5 6 7

#listapuntos = expand.grid(laidx,loidx), recordar esta forma 
# Cerramos el archivo .nc que abrimos, ya le sacamos toda la información necesaria.
nc_close(a2)

### Esta linea la agrego solamente por que estoy usando una grilla menor (3x3) a la completa (7x5), el corte
# para la otra debe tomar solo la grilla de 3x3.
laidx <- c(1,2,3)
loidx <- c(1,2,3)

# Se crea la matriz multidimensional BRCK de 3x3, que contiene dataframes para cada punto, de momento esos 
# data frames solo contienen las fechas.
BRCK = array(as.data.frame(VH, colnames = c("day")), dim=c(length(laidx),length(loidx)))
for (lo in loidx) {
  for (la in laidx) {
    as.data.frame(BRCK[[la,lo]])
  }
}

# Ahora se procede a rellenar esos data frames con cada pronóstico que le corresponda, para ello
# se recorre nuevamente la lista de archivos TRANS que contiene los nombres de los archivos .nc
for (archivo in TRANS) {
  # Primero se extrae la información correspondiente al Año, Mes y Dia-Hora de cada archivo
  AAño = substr(archivo, 1, 4)
  MMes = substr(archivo, 5, 6)
  DDiahora = substr(archivo, 7, 10)
  
  
  # se abre el archivo
  a2 = nc_open(paste0("/home/forever/Desktop/PROY/DATA/6m/prate/",archivo,"_cortado.nc",sep=""))
  
  #Ahora se van a empezar a recorrer los puntos.
  for (lo in loidx) {
    for (la in laidx) {
      # Se obtiene la fecha inicial del archivo, como también la hora inicial, igual que antes.
      tp = ncvar_get(a2,"time")
      tunitsp = ncatt_get(a2,"time","units")
      tustrp <- strsplit(tunitsp$value, " ")
      fini = unlist(tustrp)[3]
      hini = unlist(tustrp)[4]
      # Igual que antes, se crea un vector del mismo largo que los pronósticos, que va a ir de 6 en 6 horas.
      # Se convierten las horas a Chile, 00 a las 8am Chilenas.
      dh = ymd_hms(paste(fini,hini,sep=" "), tz = "UTC") - hours(12) + 6*hours(0:(length(tp)-1))
      # Se extrae la información, OJO que acá se está partiendo de lo+2 y la+1 para solo tomar el cuadro de 3x3 que encierra a la
      # región en cuestión.
      # Se transforman las unidades, ya que se encuentra en precipitación por segundo, y hay que pasar a precipitación acumulada en 6 horas
      prate = ncvar_get(a2,"prate",start=c(lo+2,la+1,1),count=c(1,1,-1))*6*3600 #ojo aca que el lo+2 y la+1 es pq estoy tomando grilla menor
      # se crea una data frame con el vector de días-horas dh y el vector de pronósticos prate.
      serie <- data.frame(dh, prate)
      # se transforma a un objeto xts, que es se puede agregar de forma mas fácil a nivel diario, como suma.
      xtsserie = xts(serie[,-1], order.by=serie[,1])
      xtsserie <- apply.daily(xtsserie,sum)
      # Para volver a data frame se utiliza la función fortify de la librería ggplot2
      df = fortify(xtsserie)
      
      # Ya agregado a nivel diario, se extrae esta prate, y se guarda en un vector.
      FCprate_diario = df$xtsserie
      
      # A continuación se crean los vectores con NA al principio y final correspondiente a cada archivo
      archivo <- as.integer(archivo)
      TRANS <- as.vector(TRANS)
      # Si el archivo que se esta leyendo es el primero. No se agregan NAs al principio, solo al final.
      
      if (archivo==TRANS[1] | archivo==TRANS[2] | archivo==TRANS[3]) {
        
        NA_final = rep(NA, LM - length(FCprate_diario))
        FCprate_diario_plusNA = c(FCprate_diario,NA_final)
        
      } else {
        
        # Se encuentran cuantos son los NA que van al final
        NA_final = rep(NA, LM - vec_NA[which(TRANS==archivo)] -length(FCprate_diario)) 
        # Se encuentran cuantos son los NA que van al principio 
        NA_principio = rep(NA, vec_NA[which(TRANS==archivo)])
        # Se agregan todos los NA al registro 
        FCprate_diario_plusNA =  c(NA_principio,FCprate_diario,NA_final)
        
      }
      
      # Finalmente se agregan estos valores al BRCK correspondiente al punto que se esta viendo.
      BRCK[[la,lo]] <- cbind.data.frame(BRCK[[la,lo]],FCprate_diario_plusNA)
      # Se le cambia el nombre a ultima columna para que sea el Año_Mes_DiaHora que corresponde al archivo
      names(BRCK[[la,lo]])[length(names(BRCK[[la,lo]]))]<- paste(AAño,"_",MMes,"_",DDiahora,sep="") #nombrar aca a la ultima columna
    }                                                                    
  }
  nc_close(a2)
}

#test = BRCK[[5]] %>% select(1:100)
#View(test)
# Ahora hay que unificar todo esto, y dejar los datos pronosticados 1 mes hacia adelante.

#rm(list = ls()[!ls() %in% c("BRCK", "laidx" , "loidx" , "lat", "lon")])
#rm(list = ls()[!ls() %in% c("BRCK")])
# i = 1; lo = 1; la = 1
# load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/brck_reforecast.RData")
#Se define una tolerancia de 0.5 mm, bajo la cual las precipitaciones se considerarán 0.
tol = 0.5

#load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/brck_reforecast.RData")
#rm(list = ls()[!ls() %in% c("BRCK","tol")])
for (i in 1:12) {
  if (i < 10) {
    assign(paste0("STATR",i), apply(BRCK, c(1,2), 
                                    function(x) {
                                      x[[1]] %>%
                                        dplyr::select(dplyr::contains(paste("_0",i,"_",sep="")), 1) %>%
                                        dplyr::filter(month(`BRCK[[la, lo]]`) == i) %>% 
                                        dplyr::select(-contains("BRCK")) %>%
                                        unlist(use.names = FALSE) %>%
                                        data.frame() %>%
                                        dplyr::filter_all(all_vars(. > tol))
                                      
                                    } ))
  } else {
    assign(paste0("STATR",i), apply(BRCK, c(1,2), 
                                    function(x) {
                                      x[[1]] %>%
                                        dplyr::select(dplyr::contains(paste("_",i,"_",sep="")), 1) %>%
                                        dplyr::filter(month(`BRCK[[la, lo]]`) == i) %>% 
                                        dplyr::select(-contains("BRCK")) %>%
                                        unlist(use.names = FALSE) %>%
                                        data.frame() %>%
                                        dplyr::filter_all(all_vars(. > tol))
                                    } ))
  }
}

rm(list = ls()[!ls() %in% c("STATR1","STATR2","STATR3","STATR4","STATR5","STATR6", #primero genero una lista 
                            "STATR7","STATR8","STATR9","STATR10","STATR11","STATR12")])

load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/brck_reforecast.RData")
rm(list = ls()[!ls() %in% c("BRCK")]) #borro la basura
#load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/brck_operational.RData")
############## inputs:
n = 12 # cantidad de ultimos datos a utilizar
dia_inicial = ymd(19940101)
dia_final = ymd(20101227)
tol = 0.5 #mas grandes que este valor se cuentan como precipitación. preguntarle a la profe si corre
#################


geopuntos = c(1,2,3,4,5,6,7,8,9)
#cantidad_geopuntos = length(laidx)*length(loidx)
#geopuntos = c(1:cantidad_geopuntos)


dates = seq(dia_inicial,dia_final, by = 'days')
dates <- as.list(dates)

#CREO UNA MATRIZ DE DATAFRAES 3x3 a rellenar
REFORECAST = array(as.data.frame(0, colnames = c("day")), dim=c(3,3)) 
for (p in geopuntos) {
  REFORECAST[[p]] = data.frame(matrix(vector(), 0, 2,
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
    
    
    if (ind*n >= n/2 ) {
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
    REFORECAST[[k]] <- rbind(REFORECAST[[k]],registro) #se bindea como row el registro
  }
}

rm(list = ls()[!ls() %in% c("REFORECAST")])

##################################
##################################
# ejemplo de gráfica de xts
library(dygraphs)
library(xts)
library(data.table)
## PASOS PARA CREAR XTS (no olvidar... )
#1) se crea un df
load("~/Desktop/PROY/Workspace/Workspace_NautilusForecast/reforecast_reforecast_12days.RData")
dfreforecast = REFORECAST[[5]]
# DEBO CORREGIR QM
dia_inicial = ymd(19950101)
dia_final = ymd(20101227)
#2) se hace esta ordenacion y listo
xtsreforecast <- xts(dfreforecast[,-1], order.by=dfreforecast[,1])

##evaluación

hest = read.csv("/home/forever/Desktop/PROY/historico_estaciones.csv", stringsAsFactors = FALSE)
hest$DateTime = mdy(hest$DateTime)
e2 = "QUINTA NORMAL SANTIAGO"
tol = 0
e = gsub('\\ ', '.', e2) 
######### primero obtengo el registro de la estación de entrada, para el mes que se busca.
registro = hest %>%
  dplyr::select(DateTime,dplyr::contains(e)) %>%
  dplyr::filter(DateTime >= dia_inicial & DateTime <= dia_final)

names(registro)[2] <- "mm"
setDT(registro)[mm<tol,mm:=0] #reemplazo valores menores a tol por cero
#########
#convierto a xts para comparar
registro <- as.data.frame(registro)
xtsobservado <- xts(registro[,-1], order.by=registro[,1])

comp <- cbind(xtsreforecast,xtsobservado)

dygraph(comp, main = paste("Comparación ",e2,sep=""))%>%
  dySeries("..1",stepPlot = T, fillGraph = T, label = "[mm] Operational") %>%
  dySeries("..2",stepPlot = T, fillGraph = T, label = "[mm] Observado") %>%
  dyLegend(width = 620) %>%
  dyRangeSelector(height = 40)






#################################### CATCHER ##############################################################################

# Este código  descarga, transforma a netCDF y corta a la resolución deseada. (archivos de las listas de url)
# Los archivos usados son forecast 9 meses

#################################################################################################################################

library(RCurl)
library(lubridate)
library(dplyr)

## ELECCIÓN DE AÑOS Y VARIABLE  
yearini = ymd(19940101)
yearfin = ymd(19951231)
variable = "prate"
#variable = "pmax"
#variable = "pmin"

## COORDENADAS DE CORTE
lat1 = -74.5
lat2 = -68
lon1 = -31.653
lon2 = -35.433


#################################### VINCULOS Y DIRECTORIOS ######################################################################

vinculoini = "https://nomads.ncdc.noaa.gov/data/cfsr-rfl-ts9/" # SOLO 9 MESES... GENERALIZAR PARA  LOS 2 QUE FALTAN
vinculofin = ".time.grb2"
MasterDir='/home/forever/Desktop/PROY/DATA/6mope/' #operational
#MasterDir='/home/forever/Desktop/PROY/DATA/6m/' #reforecast

############################### DESCARGA, CORTE Y CONVERSION ######################################################################
### aca debo generalizar la creación de esta lista, directamente leyendo desde la página 
### los archivos que hay.
url <- read.csv("/home/forever/Desktop/PROY/lista_operational_ivan.csv") #operational
#url <- read.csv("/home/forever/Desktop/PROY/lista_reforecast_ivan.csv") #reforecast
URL <- as.data.frame(url$lista) #as.vector

#a=1522
#b=8408
#URL2 <- slice(URL, a:b) 
# 2016032300 corrupto
URL2 <- as.vector(URL$`url$lista`)

miss = data.frame(matrix(vector(), 0, 1,
                  dimnames=list(c(), c("Links"))),
                  stringsAsFactors=F) #vector vacío donde se guardarán los links que no están
for (url in URL2) {
  if (!url.exists(url)) {
    miss <- rbind(miss,url) # se llena miss
    next
  }
  #DESCARGA
  download.file(url, destfile = paste(MasterDir,variable,"/",basename(url), sep = ''), 
                method="auto", quiet = FALSE, mode="wb", cacheOK = TRUE)
  #archivo = substr(url,66,75) #reforecast
  archivo = substr(url,84,93) #operational
  #CONVERSION
  #system(paste0("cd /home/forever/Desktop/PROY/DATA/6mope/prate; cdo -f nc copy ",variable,".",archivo,".time.grb2 ",archivo,".nc;",sep="")) # Reforecast
  system(paste0("cd /home/forever/Desktop/PROY/DATA/6mope/prate; cdo -f nc copy ",variable,".01.",archivo,".daily.grb2 ",archivo,".nc;",sep="")) # Operational
  
  #CORTE
  system(paste0("cd /home/forever/Desktop/PROY/DATA/6mope/prate; cdo sellonlatbox",",",lat1,",",lat2,",",lon1,",",lon2," ",archivo,".nc ",archivo,"_cortado.nc;",sep=""))#0.37s 
  
  #BORRADO DE NC Y GRB2
  system(paste0("rm ",MasterDir,variable,"/",archivo,".nc;",sep=""))
  #system(paste0("rm ",MasterDir,variable,"/",variable,".",archivo,".time.grb2",sep="")) # reforecast
  system(paste0("rm ",MasterDir,variable,"/",variable,".01.",archivo,".daily.grb2",sep="")) # operational
  }
################################## FIN

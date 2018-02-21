#################################### CATCHER ##############################################################################

# Este código  descarga, transforma a netCDF y corta a la resolución deseada. (archivos de las listas de url)
# Los archivos usados son forecast 9 meses

#################################################################################################################################
rm(list = ls()[!ls() %in% c()])
library(RCurl)
library(lubridate)
library(dplyr)
library(stringr)

## ELECCIÓN DE AÑOS Y VARIABLE  
variable = "prate"
#variable = "pmax"
#variable = "pmin"

## COORDENADAS DE CORTE
lat1 = -74.5
lat2 = -68
lon1 = -31.653
lon2 = -35.433

#################################li### VINCULOS Y DIRECTORIOS ######################################################################

MasterDir='/home/forever/Desktop/PROY/DATA/6mreal/' #Realtime

############################### DESCARGA, CORTE Y CONVERSION ######################################################################
# url <- read.csv("/home/forever/Desktop/PROY/lista_realtime_ivan.csv") # FALTA AUTOMATIZAR LA GENERACIÓN DE ESTA LISTA
# URL <- as.data.frame(url$lista) #as.vector
# URL2 <- as.vector(URL$`url$lista`)

#####NEW
system("rm /home/forever/Desktop/PROY/DATA/6mreal/prate/* ")
urlRT <- "http://nomads.ncep.noaa.gov/pub/data/nccf/com/cfs/prod/cfs/"
html <- paste(readLines(urlRT), collapse="\n")

matched <- str_match_all(html, "<a href=\"(.*?)\"")
links <- matched[[1]][, 2]
links <- as.list(links)
links <- (links[-1])

hRT <- c("00","06","12","18")
dfRT = data.frame(lista=character(), stringsAsFactors = F)
for (d in links) {
  for (h in hRT) {
    links2 = substr(d,1,12)
    links3 = substr(d,5,12)
    #aca debo crear el link que despues voy a rbindear
    linkRT = paste(urlRT,links2,"/",h,"/time_grib_01/prate.01.",links3,h,".daily.grb2",sep="")
    
    if (url.exists(linkRT)) {
      linkRT = as.data.frame(linkRT)
      dfRT <- rbind(dfRT,linkRT)
    }
  }
}
#dfRT <- dfRT[-nrow(dfRT),] #borro el ultimo registro
dfRT <- data.frame(dfRT,stringsAsFactors = F)
colnames(dfRT) <- "lista"
#####NEW
URL2 <- as.vector(dfRT$lista)


for (url in URL2) {
  #DESCARGA
  download.file(url, destfile = paste(MasterDir,variable,"/",basename(url), sep = ''), 
                method="auto", quiet = FALSE, mode="wb", cacheOK = TRUE)
  archivo = substr(url,98,107) #Real_Time
  #CONVERSION
  system(paste0("cd /home/forever/Desktop/PROY/DATA/6mreal/prate; cdo -f nc copy ",variable,".01.",archivo,".daily.grb2 ",archivo,".nc;",sep="")) # RealTime
  #CORTE
  system(paste0("cd /home/forever/Desktop/PROY/DATA/6mreal/prate; cdo sellonlatbox",",",lat1,",",lat2,",",lon1,",",lon2," ",archivo,".nc ",archivo,"_cortado.nc;",sep=""))#0.37s 
  
  #BORRADO DE NC Y GRB2
  system(paste0("rm ",MasterDir,variable,"/",archivo,".nc;",sep=""))
  
  system(paste0("rm ",MasterDir,variable,"/",variable,".01.",archivo,".daily.grb2",sep="")) # operational
}
################################## FIN

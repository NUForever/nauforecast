library(shiny)
library(leaflet)
library(ggplot2)
library(sp)
library(rgdal)
library(shinythemes)
library(lubridate)
library(dplyr)
library(xts)
library(dygraphs)
library(rgeos)
library(maptools)
library(ggplot2)
library(sp)
library(rgdal)
library(MASS)
library(raster)

library(forecast)

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  days_df  <- read.csv("days_df.csv")
  
  # token <- readRDS("droptoken.rds")
  # drop_acc(dtoken = token)
  # db_folder <- "nautilus"
  # 
  # load_db <- function() {
  #   dfrealtime <- drop_read_csv(file.path(db_folder,"dfrealtime.csv"))
  #   days_df  <- drop_read_csv(file.path(db_folder,"days_df.csv"))
  #   #r <- reactive({ drop_download(file.path(db_folder,paste(input$pronostico_raster,".tif",sep=""))) })
  # }
  

  load("statr_reforecast.RData")
  load("operational_operational_12days.RData")
  load("realtime_realtime.RData")
  #load("operational_stat.RData")
  forecast_list = list(STATR1,STATR2,STATR3,STATR4,STATR5,STATR6, 
                       STATR7,STATR8,STATR9,STATR10,STATR11,STATR12)

  est <- read.csv("est.csv", stringsAsFactors = F)
  pred <- read.csv("pred.csv", stringsAsFactors = F)
  
  est.sp <- SpatialPoints(list(est$lat,est$lon))
  pred.sp <- SpatialPoints(list(pred$lat,pred$lon))
  est$near <- apply(spDists(est.sp,pred.sp), 1, which.min)  
  hest = read.csv("historico_estaciones.csv", stringsAsFactors = FALSE)
  hest$DateTime = mdy(hest$DateTime)
  
  shape <- readOGR(dsn = "shape/cuencas.shp", layer = "cuencas")
  

  ##################### RASTER DE REALTIME ########33
  ###################################################
  
  #raster_prediccion = list.files("storms/cortadas") #antigua lista para leer raster

  dia_pronostico = reactive({ input$pronostico_raster })
  
  # r <- reactive({ raster(paste("storms/cortadas",dia_pronostico(),sep="/")) }) #este es el raster
  r <- reactive({ raster(paste("raster",dia_pronostico(),sep="/")) }) #este es el raster
  pal = colorBin("Spectral", domain = c(2,35), na.color = "transparent",reverse=T,bins = 25)
  
  #####################################
  ########################################
  
  output$mymap <- renderLeaflet({
    
    leaflet(shape) %>%
      addPolygons(fillColor = "blue",#c("green","blue","red","grey","white","yellow","pink"),
                  fillOpacity = 0,
                  weight = 2,
                  stroke = T,
                  color = "black",
                  opacity = 1,
                  popup = paste("Cuenca: ",shape$NOMBRE,sep="")) %>%
      
      addTiles() %>%
      addCircleMarkers(data = est,~lon,~lat, popup = ~as.character(paste("LAT (",est$lat,") LONG (",est$lon,") FULL:",full,sep="")), 
                       label = ~as.character(location), color = "yellow") %>%
      addCircleMarkers(data = pred,~lon,~lat, popup =~as.character(paste("LAT:",pred$lat," LONG:",pred$lon,sep="")), 
                       label = ~as.character(id), color = "blue")%>%
    addProviderTiles("Esri.WorldImagery")
      
  })


    observeEvent({
      input$estacion_seleccionada
    },{
      selectedLocation <- est[est$location == input$estacion_seleccionada, c("lat","lon")]

      leafletProxy("mymap") %>%
        setView(lng = selectedLocation$lon, lat = selectedLocation$lat, zoom = 9)

    })


    
    # observeEvent({
    #   input$bmap
    #   input$pronostico_raster
    # },{
    #   leafletProxy("mymap") %>%
    #     clearControls() %>%
    #     addProviderTiles(input$bmap) %>%
    #     addRasterImage(r(), colors = pal, opacity = 0.9) %>%
    #     leaflet::addLegend(pal = pal, values = values(r()), title = "Precipitación [mm]",
    #                        position = "bottomright")
    # })
    
    observeEvent({
      input$borrar_idw
    },{
        leafletProxy("mymap") %>%
        clearImages() %>%
        clearControls()
      })
    

    

     
########### inputs de todo ?
  #e2 <- reactive({ data_of_click$clickedMarker$label })
  e2 <- reactive({ input$estacion_seleccionada })
  #e <- reactive({ gsub('\\ ', '.', data_of_click$clickedMarker$label ) })
  e <- reactive({ gsub('\\ ', '.', input$estacion_seleccionada) })  
##########
  #mes <- reactive({input$mes_seleccionado})
  mes = reactive({as.character(month(ymd_hms(Sys.time()))) }) #ACA PONER EL BOTON  
  tol = 0 #tolerancia de 
  
  registro <- reactive({ 
      hest %>%
        dplyr::select(DateTime,dplyr::contains(e())) %>%
        dplyr::mutate(m = month(DateTime)) %>%
        dplyr::filter(m == mes()) %>%
        dplyr::filter(get(e()) > tol) %>%
        dplyr::select(contains(e()))
     }) 
    
  obsprecip <- reactive({
    sort(unlist(as.list(registro())))
  })
  
  nearest_station1 <- reactive({
    est %>%
    dplyr::filter(location == e2()) %>%
    dplyr::select(near)
  })
  
  nearest_station <- reactive({as.numeric(nearest_station1())})
  
  test <-  reactive({forecast_list[as.numeric(mes())]})
  
  modprecip <- reactive({sort(unlist(test()[[1]][[nearest_station()]]))})

#ajuste gamma con fitdistr de librería MASS

  param.observado <- reactive({ fitdistr(obsprecip(), "gamma", start=list(shape=1, scale=1), lower=0.01) })
  param.reforecast <- reactive({ fitdistr(modprecip(), "gamma", start=list(shape=1, scale=1), lower = 0.5) })

  shape.observado = reactive({ param.observado()$estimate["shape"] })
  scale.observado = reactive({ param.observado()$estimate["scale"] })
  
  shape.reforecast = reactive({ param.reforecast()$estimate["shape"] })
  scale.reforecast = reactive({ param.reforecast()$estimate["scale"] })
  
  
  Model = reactive({ c("Observado","Reforecast") })
  Scale = reactive({ c(scale.observado(),scale.reforecast()) })
  Shape = reactive({ c(shape.observado(),shape.reforecast()) })
  
  parameters = reactive({ 
    data.frame(Model(),Scale(),Shape(),stringsAsFactors = F) %>%
    setNames(c("DATA","SCALE","SHAPE"))
    })


  max.precip = reactive({max(obsprecip())})
  output$max_precip <- renderText(paste("MÁXIMA PRECIPITACIÓN OBSERVADA:",max.precip(),"mm",sep=" "))
  
  output$table1 <- renderTable({ parameters() }, align = 'c')
  
  cols <- c("LINE1"="blue","LINE2"="red")
  df <- reactive ({ data.frame(x=seq(0, max.precip(), 0.1)) })
  
  output$cdfplot <- renderPlot(
  ggplot(data=df(), aes(x=x)) +
    theme(
       panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.box.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank())+
      #rect = element_rect(fill = "transparent") ) + 
    stat_function(aes(colour = "LINE1"), fun=pgamma, args=list(shape = shape.observado(), scale = scale.observado())) +
    stat_function(aes(colour = "LINE2"), fun=pgamma, args=list(shape = shape.reforecast(), scale = scale.reforecast())) +
    xlab("Precipitación") + ylab("F(x)") +
    scale_colour_manual("Ajuste Gamma: ", values=c(LINE1="red",LINE2="blue"),
                        labels = c("Observado","Reforecast")) +
    scale_y_continuous(labels=percent),
  bg = 'transparent'
  )
  
########################################
# Corrección y estadígrafos de desempeño
########################################
  
  data_operational = reactive({ 
    OPERATIONAL[[nearest_station()]] %>%
      dplyr::filter(month(k2)==mes()) %>%
      dplyr::select(mm)
  })
  data_operational2 <- reactive({ unlist(data_operational()) })  
  
  v = reactive({ 
    pgamma(data_operational2(),scale=scale.reforecast(),shape=shape.reforecast()) 
  })
  
  operational_corregido = reactive({
    qgamma(v(),scale=scale.observado(),shape=shape.observado())
  })
  
  dia_ini = ymd(20110331)
  dia_fin = ymd(20161231)

  registro2 <- reactive({
    hest %>%
      dplyr::select(DateTime,dplyr::contains(e())) %>%
      dplyr::mutate(m = month(DateTime)) %>%
      dplyr::filter(m == mes()) %>%
      dplyr::filter(DateTime >= dia_ini) %>%
      dplyr::filter(DateTime <= dia_fin) %>%
      dplyr::select(contains(e()))
  })

  df_comp = reactive ({ cbind(operational_corregido(),registro2()) %>%
      setNames(c("operational","registro")) })

  tol_ope = 0.5
  tol_reg = 0

  estadigrafos = reactive({ df_comp() })
  estadigrafos2 <- reactive ({ estadigrafos() %>%
      mutate(HIT = (operational>tol_ope & registro>tol_reg),
             MISS = (operational<tol_ope & registro>tol_reg),
             FA = (operational>tol_ope & registro<=tol_reg),
             CN = (operational<=tol_ope & registro<=tol_reg)) %>%
      dplyr::select(HIT,MISS,FA,CN) })


  estadigrafos4 <- reactive({ estadigrafos2() %>%
      dplyr::summarise(nHIT = sum(HIT=="TRUE"),
                       nMISS = sum(MISS=="TRUE"),
                       nFA = sum(FA=="TRUE"),
                       nCN = sum(CN=="TRUE")) })

  output$tablacontingencia <- renderTable({ estadigrafos4() })
  
  # estadigrafos
  
  nHIT = reactive({ 
    if (class(estadigrafos4()$nHIT)=='NULL') {
      0 } else {
        estadigrafos4()$nHIT
      }
    })
  nMISS = reactive({ 
    if (class(estadigrafos4()$nMISS)=='NULL') {
      0 } else {
        estadigrafos4()$nMISS
      }
  }) 
  nFA = reactive({ 
    if (class(estadigrafos4()$nFA)=='NULL') {
      0 } else {
        estadigrafos4()$nFA
      }
  }) 
  nCN = reactive({ 
    if (class(estadigrafos4()$nCN)=='NULL') {
      0 } else {
        estadigrafos4()$nCN
      }
  }) 

  total = reactive({ nHIT()+nMISS()+nFA()+nCN() })
  
  
  ACCURACY  = reactive({ (nHIT()+nCN())/total() })
  BIAS      = reactive({ (nHIT()+nFA())/(nHIT()+nMISS()) })
  POD       = reactive({ nHIT()/(nHIT()+nMISS()) })
  FAR       = reactive({ nFA()/(nHIT()+nFA()) })
  POFD      = reactive({ nFA()/(nCN()+nFA()) })
  SR        = reactive({ nHIT()/(nHIT()+nFA()) })


  indicadores = reactive({ 
    data.frame(ACCURACY(),BIAS(),POD(),FAR(),POFD(),SR(),stringsAsFactors = F) %>%
      setNames(c("Accuracy","Bias Score","Hit Rate","FA Ratio","FA Rate","Success Ratio"))
  })
  
  output$tablaindicadores <- renderTable({ indicadores() })
  
  
  
  ACC = reactive({ accuracy(df_comp()$operational,df_comp()$registro) })
  ACC_DF = reactive({ as_data_frame(ACC()) })
  

  
  output$tablaindicadores2 <- renderTable({ ACC_DF() })
  
  ##########################
  ## Real Time
  #acá debo dejar que lea de forma remota el archivo u OBJETO REALTIME (creo que el archivo está antes)
  ##########################
  
  mes_actual = ymd_hms(Sys.time())
  mes_actual <- month(mes_actual)
  
  data_realtime = reactive({ 
    REALTIME[[nearest_station()]] %>%
      dplyr::select(mm)
    
  })
  data_realtime2 <- reactive({ unlist(data_realtime()) })  

  #PARAMETROS###########
  registro_realtime <- reactive({ 
    hest %>%
      dplyr::select(DateTime,dplyr::contains(e())) %>%
      dplyr::mutate(m = month(DateTime)) %>%
      dplyr::filter(m == mes_actual) %>%
      dplyr::filter(get(e()) > tol) %>% 
      dplyr::select(contains(e()))
  }) 
  obsprecip_realtime <- reactive({
    sort(unlist(as.list(registro_realtime())))
  })
  
  test_realtime <-  reactive({forecast_list[as.numeric(mes_actual)]})
  modprecip_realtime <- reactive({sort(unlist(test_realtime()[[1]][[nearest_station()]]))})
  
  #ajuste gamma con fitdistr de librería MASS
  
  param.observado_realtime <- reactive({ fitdistr(obsprecip_realtime(), "gamma", start=list(shape=1, scale=1), lower=0) })
  param.reforecast_realtime <- reactive({ fitdistr(modprecip_realtime(), "gamma", start=list(shape=1, scale=1), lower = tol) })
  
  shape.observado_realtime = reactive({ param.observado_realtime()$estimate["shape"] })
  scale.observado_realtime = reactive({ param.observado_realtime()$estimate["scale"] })
  
  shape.reforecast_realtime = reactive({ param.reforecast_realtime()$estimate["shape"] })
  scale.reforecast_realtime = reactive({ param.reforecast_realtime()$estimate["scale"] })
  
  vrt = reactive({ 
    pgamma(data_realtime2(),scale=scale.reforecast_realtime(),shape=shape.reforecast_realtime()) 
  })
  
  realtime_corregido = reactive({
    qgamma(vrt(),scale=scale.observado_realtime(),shape=shape.observado_realtime())
  })
  
  realtimedates = REALTIME[[1]]$k2
  
  dfrealtime = reactive({ data_frame(realtimedates,realtime_corregido()) })
  dfrealtime2 = reactive({ as.data.frame(dfrealtime()) })
  
  
  xtsrealtime <- reactive({ xts(dfrealtime2()[,-1], order.by=dfrealtime2()[,1]) })
  
  output$realtime <- renderDygraph({
    dygraph(xtsrealtime(), main = "Precipitación Pronosticada") %>%
    dySeries(stepPlot = T, fillGraph = T, label = "mm") %>%
    dyLegend(width = 620) %>%
    dyRangeSelector(height = 40) 
    })
  
  #descarga de datos
  data_forecast <- read.csv("dfrealtime.csv")
  output$descargar_forecast <- downloadHandler(
       filename = "data_forecast.csv",
       content = function(con) {
         write.csv(data_forecast, con)
       }
     )
  #desplegar datos
  data_forecast_reactive <- eventReactive(input$go, {
    data_forecast
  })
  
  output$tabla_forecast <- renderTable({ data_forecast_reactive() })
  
  #pop-up
  # observeEvent(input$mymap_marker_click, {
  #   id = input$mymap_marker_click$id
  #   showModal(modalDialog(
  #     title = "Pronóstico y estadígrafos", easyClose = TRUE
  #   )
  #   bsModal()
  #   )
  # })
  
  
  
  
  
  
  

}
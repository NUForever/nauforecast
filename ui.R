library(shiny)
library(leaflet)
library(ggplot2)
library(dygraphs)
library(rgdal)
library(ggfortify)
library(MASS)
library(rgeos)
library(rdrop2)
library(shinythemes)

est <- read.csv("est.csv", stringsAsFactors = F)
pred <- read.csv("pred.csv", stringsAsFactors = F)
raster_prediccion = list.files("raster")
days_df  <- read.csv("days_df.csv")

meses <- c("1","2","3","4","5","6","7","8","9","10","11","12")
fluidPage(theme = shinytheme("slate"),
tags$head(HTML('<link rel="icon", href="mylogo.png", 
                                   type="image/png" />')),
navbarPage(
  windowTitle="NAUTILUS forecast",
           
  title=div(img(src="mylogo.png")), id="nav",
           tabPanel("MAP",
                   HTML('<body background="CARGANDO.png" height="15" width="15" align="mid le">
                         </body>'),
                    HTML('<svg width="1900" height="900"></svg>
                         <script src="https://d3js.org/d3.v4.min.js"></script>
                         <script>
                         
                         var svg = d3.select("svg"),
                         width = +svg.attr("width"),
                         height = +svg.attr("height"),
                         angles = d3.range(0, 2 * Math.PI, Math.PI / 200);
                         
                         var path = svg.append("g")
                         .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")")
                         .attr("fill", "none")
                         .attr("stroke-width", 10)
                         .attr("stroke-linejoin", "round")
                         .selectAll("path")
                         .data(["midnightblue", "dodgerblue", "white"])
                         .enter().append("path")
                         .attr("stroke", function(d) { return d; })
                         .style("mix-blend-mode", "lighten")
                         .datum(function(d, i) {
                         return d3.radialLine()
                         .curve(d3.curveLinearClosed)
                         .angle(function(a) { return a; })
                         .radius(function(a) {
                         var t = d3.now() / 1000;
                         return 200 + Math.cos(a * 8 - i * 2 * Math.PI / 3 + t) * Math.pow((1 + Math.cos(a - t)) / 2, 3) * 32;
                         });
                         });
                         
                         d3.timer(function() {
                         path.attr("d", function(d) {
                         return d(angles);
                         });
                         });
                         
                         </script>'),
                    
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("mymap", width = "100%", height = "100%"),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
                                      draggable = F, top = "80px", left = "0px", right = "auto", bottom = "auto",
                                      width = "750px", height = "1200px",
                                      h3(" "),
                                      h3(" "),
                                      h3(" "),
                                      selectInput(inputId = "estacion_seleccionada", label = "Seleccione estación",
                                                  choices = est$location, selected = "CERRO CALAN"),
                                      #selectInput(inputId = "mes_seleccionado", label = "Seleccione mes",
                                      #            choices = meses, selected = 1),
                                      textOutput("max_precip"),
                                      tableOutput("table1"),
                                      #plotOutput('cdfplot', heigh = 300),
                                      textOutput("Tabla de contingencia"),
                                      tableOutput("tablacontingencia"),
                                      tableOutput("tablaindicadores"),
                                      tableOutput("tablaindicadores2"),
                                      dygraphOutput("realtime", height = "350px")
                        ),
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = T,
                                      draggable = T, top = "80px", left = "400px", bottom = "auto",
                                      width = "350px", height = "auto",
                                      h3(" "),
                                      selectInput(inputId = "pronostico_raster", label = "Interpolar pronósticos diarios (IDW)",
                                                  choices = raster_prediccion, selected = 8),
                                      actionButton(inputId = "borrar_idw", label = "Limpiar"),
                                      #actionButton(inputId = "go","Ver pronósticos"),
                                      #bsModal("modalExample", "Ver Pronósticos", "go", size = "small",tableOutput("tabla_forecast"), easyClose = TRUE),
                                      downloadButton(outputId = "descargar_forecast", label = "Descargar pronósticos"),
                                      selectInput("bmap", "Capa Base", choices = c("OpenTopoMap", 
                                                                                                "NASAGIBS.ModisTerraSnowCover",
                                                                                                "Esri.WorldImagery",
                                                                                                "OpenWeatherMap_Clouds",
                                                                                                "HERE.hybridDay"),
                                                  selected = "Esri.WorldImagery")
                        ),
                        
                        tags$div(id="cite",
                                 'Escuela de Ingeniería Civil', tags$em('PUCV')
                        )
                    )
           )
  # tabPanel("HOME",
  #          HTML('<body background="eic.png" height="15" width="15" align="middle">
  #               </body>'),
  #          HTML('<meta charset="utf-8">
  #               <body>
  #               <script src="//d3js.org/d3.v3.min.js"></script>
  #               <script>
  #               
  #               var width = 1900,
  #               height = 900,
  #               τ = 2 * Math.PI,
  #               maxLength = 80,
  #               maxLength2 = maxLength * maxLength;
  #               
  #               var nodes = d3.range(600).map(function() {
  #               return {
  #               x: Math.random() * width,
  #               y: Math.random() * height
  #               };
  #               });
  #               
  #               var force = d3.layout.force()
  #               .size([width, height])
  #               .nodes(nodes.slice())
  #               .charge(function(d, i) { return i ? -30 : -1500; })
  #               .on("tick", ticked)
  #               .start();
  #               
  #               var voronoi = d3.geom.voronoi()
  #               .x(function(d) { return d.x; })
  #               .y(function(d) { return d.y; });
  #               
  #               var root = nodes.shift();
  #               
  #               root.fixed = true;
  #               
  #               var canvas = d3.select("body").append("canvas")
  #               .attr("width", width)
  #               .attr("height", height)
  #               .on("ontouchstart" in document ? "touchmove" : "mousemove", moved);
  #               
  #               var context = canvas.node().getContext("2d");
  #               
  #               function moved() {
  #               var p1 = d3.mouse(this);
  #               root.px = p1[0];
  #               root.py = p1[1];
  #               force.resume();
  #               }
  #               
  #               function ticked() {
  #               var links = voronoi.links(nodes);
  #               
  #               context.clearRect(0, 0, width, height);
  #               
  #               context.beginPath();
  #               for (var i = 0, n = links.length; i < n; ++i) {
  #               var link = links[i],
  #               dx = link.source.x - link.target.x,
  #               dy = link.source.y - link.target.y;
  #               if (dx * dx + dy * dy < maxLength2) {
  #               context.moveTo(link.source.x, link.source.y);
  #               context.lineTo(link.target.x, link.target.y);
  #               }
  #               }
  #               context.lineWidth = 2;
  #               context.strokeStyle = "#99ccff";
  #               context.stroke();
  #               
  #               context.beginPath();
  #               for (var i = 0, n = nodes.length; i < n; ++i) {
  #               var node = nodes[i];
  #               context.moveTo(node.x, node.y);
  #               context.arc(node.x, node.y, 2, 0, τ);
  #               }
  #               context.lineWidth = 1;
  #               context.strokeStyle = "#000099";
  #               context.stroke();
  #               <!--COLOR DE LAS PELOTITAS-->
  #               context.fillStyle = "#000099";
  #               context.fill();
  #               }
  #               
  #               </script>'))
  # 

)
)

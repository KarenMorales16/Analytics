library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(dbplyr)
library(tidyverse)
library(sqldf)
library(shiny)
library(DT)
library(shinydashboard)
#library(sf)
library(shinymaterial)
library(gdata)
library(highcharter)
library(sf)
library(ggplot2)
library(mapview)

help(highcharter)

#install.packages("highcharter")


#DataSet Inicial
Ordenes_volumen = data.frame()
Tiempo_de_entrega= data.frame()
onTime= data.frame()
DataOntime= data.frame()
Texto="6"

ListaFillRate= list("All")



# Define UI
ui <- dashboardPage(
  
  dashboardHeader(title = h4("Control AMECAR ")),
  dashboardSidebar(  
    
    fluidPage(
      
      fileInput('target_upload', 'Choose xlsx file',
                accept = c(".xlsx")
      ),  
      uiOutput('ListaonTime'),
    )
  ),
  
  dashboardBody( 
    fluidRow(
      tabBox(
        title = "FillRate & On Time ",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "100%", width = 800,
        tabPanel("Orden de pedido ", DT::dataTableOutput("TablaOrden") ),
        tabPanel("Top proveedores",DT::dataTableOutput("TablaProveedores")),
        tabPanel("Grafica Fill rate por semana",
        box(width=8,status = "primary", solidHeader = F,align="center",plotlyOutput("graphFillRate"),br(),
              shinyWidgets::sliderTextInput("obs","Valor Fill Rate:",
                                               choices=c(0, 0.1, 0.2, 0.03,0.4,0.5,0.6,0.8,0.9,1,2,3,4,5,6),
                                               selected=0, grid = T))
                 ,
      fluidRow(h3("Top de Fill rate proveedores" ,align = 'center'),
               p("Proveedor ULINE SHIPPING SUPPLIES, Articulo BRUSH TOILET, Fill rate de 6" ),
               p("Proveedor ESTRADA REZA JESUS EDUARDO SUPPLIES, Articulo TE HELADO , Fill rate de 4" ),
               p("Proveedor SHAMROCK FOODS COMPANY, Articulo CRISS CUT FRIES 6 / 5.0 / CS , Fill rate de 3")
        ),
      box(title = "Tabla de Fill Rate",status = "primary",solidHeader = F,collapsible = T,width = 12,column(12, align="center", DT::dataTableOutput("TablaFillRateSemanal")))),
        tabPanel("Grafica Proveedores",plotlyOutput("graphProVolumen"),plotlyOutput("graphMex"),DT::dataTableOutput("TablaTopProveedores")))),uiOutput('value')
        )
  
  
)



server <- function(input, output) {

  #Imprimir lista de ListBox ONTIME
  output$ListaonTime <- renderUI({
    selectInput("choiceonTime", label =  h5("ON TIME"), 
                choices = c("All","100%","0%"), 
                selected = 1)
  })
  
  
  #TABLA ON TIME
  output$TablaOrden<- DT::renderDataTable({
    
    table_Datos <- df_products_upload()
    inFile <- input$target_upload
    # Descarga el archivo zip de github
    download.file(url = "https://github.com/prestevez/covid-19-mx-map/raw/master/datos_covid/01_32_mun.zip",destfile = "datos_covid/01_32_mun.zip")
    
    # Extrae el archivo zip en la carpeta datos_covid
    unzip("datos_covid/01_32_mun.zip",
          exdir = "datos_covid/")
    
    # Carga el archivo shapefile a R
    ctes <<- st_read("datos_covid/01_32_mun.shp")
    if (is.null(inFile))
      return(NULL)
    DT::datatable(
      if(input$choiceonTime != "All") { DataOntimeF<-DataOntime[DataOntime$OnTime == input$choiceonTime,]  }
      else {  DataOntimeF<-DataOntime }
    )%>% formatStyle("OnTime", backgroundColor = styleEqual("0%", "red")) %>% 
      formatStyle("OnTime", backgroundColor = styleEqual("100%", "lightgreen")) %>% 
      formatStyle("Fill rate",   background = styleColorBar(DataOntime$"Fill rate", 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # TABLA PROVEEDORES
  output$TablaProveedores<- DT::renderDataTable({
    table_Datos <- df_products_upload()
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    DT::datatable(TopProVolumen) %>% 
      formatStyle("VolumenTotal",   background = styleColorBar(TopProVolumen$"VolumenTotal", 'lightgreen'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  
  # Tabla de Fill Rate Semanal
  output$TablaFillRateSemanal<- DT::renderDataTable({
    inFile <- input$target_upload
    Obsr <-input$obs
    if (is.null(inFile))
      return(NULL)
   
    DT::datatable( GraficonFile[GraficonFile$`Fill rate` ==Obsr ,])%>%
      formatStyle("Semana", backgroundColor = styleEqual("1", 'rgb(0, 160, 178)')) %>%
      formatStyle("Semana", backgroundColor = styleEqual("2", 'rgb(0, 156, 137)')) %>%
      formatStyle("Semana", backgroundColor = styleEqual("3", 'rgb(200, 216, 79)')) %>%
      formatStyle("Semana", backgroundColor = styleEqual("4", 'rgb(133, 176, 70)')) %>%
      formatStyle("Semana", backgroundColor = styleEqual("5", 'rgb(0, 139, 107)')) 
  })
  
  # Tabla Top Proveedores
  output$TablaTopProveedores<- DT::renderDataTable({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    DT::datatable(GraficoProVolumenTotal) %>% 
      formatStyle("VolumenTotal",   background = styleColorBar(GraficoProVolumenTotal$"VolumenTotal", 'lightgreen'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    
  })
  
  
  
  
  #Seleccionar datos tabla
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    
    Ordenes_volumen <<- read_excel(inFile$datapath, sheet = "Ordenes_volumen")
    Tiempo_de_entrega <<- read_excel(inFile$datapath, sheet = "Tiempo_de_entrega")
    
    
    FillRate<-Ordenes_volumen$"Fill rate"
    QtySolicitada<-Ordenes_volumen$"Sum Cantidad Original"
    QtyRecibida<- Ordenes_volumen$"Sum Cantidad Recibida Oracle"
    
    
    
    colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "# orden de compra")] <-"OrdendeCompra"
    colnames(Tiempo_de_entrega)[which(names(Tiempo_de_entrega) == "# Orden de compra")]<-"OrdendeCompra"
    colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Sum Cantidad Recibida Oracle")]<-"QtyRecibida"
    colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Artículo")]<-"Articulo"
    colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Descripción Artículo")]<-"Descripcion"
    
    
    # Calcular FillRate
    QtyFillRate <-(QtyRecibida/QtySolicitada)
    
    # Calcular Ontime
    Tiempo_de_entrega$OnTime<-(Tiempo_de_entrega$`Fecha Promesa Orignal`)>=(Tiempo_de_entrega$`Fecha Recepción Oracle`)
    Tiempo_de_entrega$OnTime[Tiempo_de_entrega$OnTime ==TRUE] <- "100%"
    Tiempo_de_entrega$OnTime[Tiempo_de_entrega$OnTime ==FALSE] <- "0%"
    
    Ordenes_volumen$`Fill rate`<-as.numeric(sprintf("%.1f",QtyFillRate))
    d1<-Ordenes_volumen[,1:8]
    d2<-select(Tiempo_de_entrega,"OrdendeCompra",OnTime,"Fecha Promesa Orignal","Fecha Recepción Oracle")
    d3<-left_join(d1, distinct(d2), by="OrdendeCompra")
    
    #ListaRates<<- as.list(sqldf("SELECT `Fill rate` FROM Ordenes_volumen where `Fill rate` is not Null group by `Fill rate` "))
    
    
    colnames(d3)[which(names(d3) == "Fecha Recepción Oracle")]<-"Fecha Recepción"
    colnames(d3)[which(names(d3) == "Fecha Promesa Orignal")]<- "Fecha compromiso"
    colnames(d3)[which(names(d3) == "Sum Cantidad Original")]<- "Qtycompromiso"
    colnames(d3)[which(names(d3) == "OrdendeCompra")]<- "#Orden"
    
    d3$"Fecha Recepción"<-as.Date(d3$"Fecha Recepción",format="%d-%b-%y")
    d3$"Fecha compromiso"<-as.Date(d3$"Fecha compromiso",format="%d-%b-%y")
    
    DataOntime<<- d3
    
    DataOntime<- d3
    
    ### Ctrl+Shift+M %>% 
    # Con el mutate nos ayuda a mutar las columnas en el dataset, cuando la columna esta en nula se elimina, str_like nos ayuda a colocarle una condicion  a un string
    DataOntime <-DataOntime %>% 
      mutate(Cuidad = matrix((unlist(strsplit(Ordenes_volumen$CEDIS, "CEDIS"))), ncol = 2, byrow = TRUE)[,2]) 
    DataOntime <-DataOntime %>%
      mutate(Cuidad = str_replace(DataOntime$Cuidad,"HMO MERLOT","HERMOSILLO")) 
    DataOntime <-DataOntime %>% 
      mutate(cve_ent = ifelse(str_like(DataOntime$Cuidad ,'%HERMOSILLO%') ,"26030",ifelse(str_like(DataOntime$Cuidad ,'%TIJUANA%') ,"02004",ifelse(str_like(DataOntime$Cuidad ,'%GUADALAJARA%') ,"14039",NA))))
    #mutate(Cuidad = str_replace(DataOntime$Cuidad,"HMO MERLOT","HERMOSILLO")) 
    Volumen<-sqldf("SELECT cve_ent,Cuidad,Proveedor,SUM(QtyRecibida) as poblacion FROM DataOntime GROUP BY Proveedor,Cuidad  ORDER by poblacion DESC")
    
    
    
    datos_mun <<- read_csv("https://raw.githubusercontent.com/prestevez/covid-19-mx-map/master/datos_covid/Casos_Diarios_Estado_Nacional_Confirmados.csv")
    
    
    CompraVolumen<<-sqldf("SELECT cve_ent,Cuidad,SUM(QtyRecibida) as poblacion FROM DataOntime GROUP BY Cuidad  ORDER by poblacion DESC")
    
    
    # TOP 5 DE Provedores por volumen
    #Renombrar Columna
    colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Sum Cantidad Recibida Oracle")]<-"QtyRecibida"
    colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Artículo")]<-"Articulo"
    colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Descripción Artículo")]<-"Descripcion"
    
    
    TopProVolumen<<-
      sqldf("SELECT Articulo,Descripcion,Proveedor,SUM(QtyRecibida) as VolumenTotal FROM Ordenes_volumen GROUP BY Proveedor,Articulo  ORDER by VolumenTotal DESC")
    
    graphTopProVolumen<<-
      sqldf("SELECT Articulo,Descripcion,Proveedor,SUM(QtyRecibida) as VolumenTotal FROM Ordenes_volumen GROUP BY Proveedor  ORDER by VolumenTotal DESC")
    
    
    
    ##GRAFICA ONFILES POR SEMANA
    GraficonFile<-select(DataOntime,Proveedor,"Descripcion","QtyRecibida","Qtycompromiso","Fecha Recepción","Fill rate")
    GraficonFile$Semana<-
      case_when(
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-05-01")	& GraficonFile$`Fecha Recepción`<=	as.Date("2023-05-07")	 ~ '1',
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-05-08")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-05-14")	 ~ '2',
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-05-15")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-05-21")  ~ '3',	
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-05-22")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-05-28")	 ~ '4',	
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-05-29")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-06-04")	 ~ '5',	
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-06-05")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-06-11")	 ~ '6',	
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-06-12")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-06-18")	 ~ '7',	
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-06-19")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-06-25")	 ~ '8',	
        GraficonFile$`Fecha Recepción`>=	as.Date("2023-06-26")	&	GraficonFile$`Fecha Recepción`<=	as.Date("2023-07-02")	 ~ '9')
    
    ## AQUI VA
  
    GraficonFile <<- GraficonFile[with(GraficonFile, order(-GraficonFile$`Fill rate`)), ]
    GraficoFinalFile1<<-sqldf("SELECT Semana,`Fill rate` as FillRate,count(`Fill rate`) as FillRateTotal  FROM GraficonFile  where Semana is not NULL and `Fill rate`<=1  group by Semana,`Fill rate` order by Semana desc")
    GraficoFinalFile<<-sqldf("SELECT Semana,`Fill rate` as FillRate,count(`Fill rate`) as FillRateTotal  FROM GraficonFile  where Semana is not NULL and `Fill rate`<=1  group by Semana,`Fill rate` order by Semana asc")
    
    Semana1<<-GraficoFinalFile[GraficoFinalFile$Semana == "1",]
    Semana2<<-GraficoFinalFile[GraficoFinalFile$Semana == "2",]
    Semana3<<-GraficoFinalFile[GraficoFinalFile$Semana == "3",]
    Semana4<<-GraficoFinalFile[GraficoFinalFile$Semana == "4",]
    Semana5<<-GraficoFinalFile[GraficoFinalFile$Semana == "5",]
    Semana6<<-GraficoFinalFile[GraficoFinalFile$Semana == "6",]
    Semana7<<-GraficoFinalFile[GraficoFinalFile$Semana == "7",]
    Semana8<<-GraficoFinalFile[GraficoFinalFile$Semana == "8",]
    Semana9<<-GraficoFinalFile[GraficoFinalFile$Semana == "9",]
    
    #Top proveedores por volumen
    GraficoProVolumen<<-sqldf("SELECT Proveedor,SUM(QtyRecibida) as VolumenTotal FROM Ordenes_volumen GROUP BY Proveedor  ORDER by VolumenTotal DESC LIMIT 10")
    
    #Top proveedores por volumen
    GraficoProVolumenTotal<<-sqldf("SELECT Proveedor,SUM(QtyRecibida) as VolumenTotal FROM Ordenes_volumen GROUP BY Proveedor  ORDER by VolumenTotal DESC")
    
    
    
  })
  
  #Graficas Fill Rate
  output$graphFillRate <- renderPlotly({
    table_Datos <- df_products_upload()
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    fig <- GraficoFinalFile %>% plot_ly()
    fig <- fig %>% add_trace(x =  ~Semana1$FillRate , y = ~Semana1$FillRateTotal , type = 'bar', name='01 May - 07 May',
                             text = ~Semana1$FillRateTotal, textposition = 'auto',
                             marker = list(color = 'rgb(0, 160, 178)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))  
    
    fig <- fig %>% add_trace(x =  ~Semana2$FillRate , y = ~Semana2$FillRateTotal , type = 'bar', name='08 May - 14	May',
                             text = ~Semana2$FillRateTotal, textposition = 'auto',
                             marker = list(color = 'rgb(0, 156, 137)',
                                           line = list(color = 'rgb(24,23,23)', width = 0)))  
    
    fig <- fig %>% add_trace(x =  ~Semana3$FillRate , y = ~Semana3$FillRateTotal , type = 'bar', name='15 May - 21	May',
                             text = ~Semana3$FillRateTotal, textposition = 'auto',
                             marker = list(color = 'rgb(200, 216, 79)',
                                           line = list(color = 'rgb(24,23,23)', width = 0)))
    
    fig <- fig %>% add_trace(x =  ~Semana4$FillRate , y = ~Semana4$FillRateTotal , type = 'bar', name='22 May - 28	May ',
                             text = ~Semana4$FillRateTotal, textposition = 'auto',
                             marker = list(color = 'rgb(133, 176, 70)',
                                           line = list(color = 'rgb(24,23,23)', width = 0)))
    
    fig <- fig %>% add_trace(x =  ~Semana5$FillRate , y = ~Semana5$FillRateTotal , type = 'bar', name='29 May - 04 Jun',
                             text = ~Semana5$FillRateTotal, textposition = 'auto',
                             marker = list(color = 'rgb(0, 139, 107)',
                                           line = list(color = 'rgb(24,23,23)', width = 0))) 
    
    
    fig <- fig %>% layout(title = "Fill rate Mayo",
                          barmode = 'group',
                          xaxis = list(title = "% Fill rate"),
                          yaxis = list(title = "Quantity"))
    
    fig
    
    
  })
  
  #Grafica Proveedores
  output$graphProVolumen <- renderPlotly({
    table_Datos <- df_products_upload()
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    figProVolumen <- GraficoProVolumen %>% plot_ly( ) %>%
      add_trace(x = ~GraficoProVolumen$VolumenTotal, y = ~GraficoProVolumen$Proveedor, type = 'bar',  width=1 ,orientation = 'h',
                text = ~GraficoProVolumen$Proveedor,textposition = 'top center',
                texttemplate = "%{y} Qty: %{x:.1f}",
                marker = list( color="rgb(0, 160, 178)",
                               line = list(color = 'rgb(0, 143, 189)', width = 2))) %>%
      layout(title = "<b>Top 10 proveedores por volumen",
             xaxis = list(title = ""),
             yaxis = list(title = "Proveedor",zeroline = FALSE,showline = FALSE,showticklabels = FALSE))
    
    figProVolumen
  })
  #Graficas Fill Rate
  output$graphMex <- renderPlotly({
    table_Datos <- df_products_upload()
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    # vector con los puntos de corte
    brks <- c(0,1000,6000,11000,21000,51000,91000,101000,121000,
              max(CompraVolumen$poblacion, na.rm = TRUE))
    
    mex_map_covid <<- ctes %>%
      # unir tablas
      left_join(CompraVolumen,
                # indicar explícitamente las columnas índice,
                # necesario cuando no tienen el mismo nombre
                by = c("CVEGEO" = "cve_ent")) 
    
    
    mex_map_covid %>%
      # usamos el aesthetic fill para indicar la columna de casos
      ggplot(aes(fill = poblacion)) 
    #geom_sf()
    
    mex_map_covid %>%
      # usamos dplyr para generar una nueva columna
      mutate(casos_cut = cut(poblacion, breaks = brks)) %>%
      # usamos el aesthetic fill para indicar la columna de casos_cut
      ggplot(aes(fill = casos_cut)) +
      
      # cambia el color y el grosor de las líneas
      geom_sf(colour = "gray", size = 0.01  )+
      geom_sf_label(aes(label = poblacion), size  = 2, label.size = NA)+ 
      
      # agrega títulos
      labs(x="", y="",title = "Volumen de Consumo CEDIS AMECAR",
           subtitle = "Actualizado al 17 de Julio 2023")+
      # caption = "Datos: https://www.gob.mx/salud")
      
      # cambia el color de relleno, nota que es una función distinta
      scale_fill_brewer( "CONSUMO",palette = "Dark2",na.value = "lightgray") +
      # retirar el fondo gris
      theme_bw()
    
  })
  
}












# Run the application 
shinyApp(ui = ui, server = server)

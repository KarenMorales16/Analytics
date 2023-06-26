
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(dbplyr)
library(sqldf)
library(shiny)
library(DT)
library(shinydashboard)
library(sf)
library(shinymaterial)
library(gdata)


#DataSet Inicial
Ordenes_volumen = data.frame()
Tiempo_de_entrega= data.frame()
onTime= data.frame()
DataOntime= data.frame()

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
        tabPanel("Grafica Fill rate por semana", plotlyOutput("graphFillRate"),DT::dataTableOutput("TablaFillRateSemanal")),
        tabPanel("Grafica Proveedores",plotlyOutput("graphProVolumen"),DT::dataTableOutput("TablaTopProveedores"))
        
      )),uiOutput('value')
    
    
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
     if (is.null(inFile))
       return(NULL)
     DT::datatable(GraficonFile)%>%
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
    
    
    
    colnames(Ordenes_volumen)[1] <-"OrdendeCompra"
    colnames(Tiempo_de_entrega)[3] <-"OrdendeCompra"
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


}

  
  



  


  
 
  
# Run the application 
shinyApp(ui = ui, server = server)


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




#DataSet Inicial
Ordenes_volumen = data.frame()
Tiempo_de_entrega= data.frame()
onTime= data.frame()
DataOntime= data.frame()

ListaFillRate= list("All")


#Grafica In Time /Out Time
corridas_ano = data.frame()

#Grafica In Time /Out Time
MothDetail =data.frame()

#Grafica In Time /Out Time
Outoftime = data.frame()


# Define UI
ui <- dashboardPage(
  
  dashboardHeader(title = h4("Control AMECAR ")),
  dashboardSidebar(  
    
    fluidPage(
      
      fileInput('target_upload', 'Selecciona Excel Prueba de Indicadores',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv')
      ),  
    
    )
  ),
  
  dashboardBody( 
    fluidRow(
      tabBox(
        title = "FillRate & On Time ",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "100%", width = 800,
        tabPanel("Orden de pedido ",uiOutput('ListaonTime'),uiOutput('ListaFillRatesBox'), DT::dataTableOutput("TablaOrden") ),
        #tabPanel("Acumulated Hours ",plotlyOutput("graphHoras") ,DT::dataTableOutput("sample_table3")),
        #tabPanel("Montly overview", plotlyOutput("graph1") , DT::dataTableOutput("sample_table1")),
        #tabPanel("Out of time",plotlyOutput("graph2"),DT::dataTableOutput("sample_table2"))
        
      )),uiOutput('value')
    
    
  )
  
  
)



server <- function(input, output) {
  #Imprimir lista de ListBox ONTIME
  output$ListaonTime <- renderUI({
    selectInput("choiceonTime", label = h5("ON TIME"), 
                choices = c("All","100%","0%"), 
                selected = 1)
  })
  
  output$ListaFillRatesBox<- renderUI({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    selectInput("choiceFillRate", label = h5("Fill Rates"), 
                choices = ListaRates, 
                selected = 1)
  })
  
  
   output$TablaOrden<- DT::renderDataTable({
     table_Datos <- df_products_upload()
     inFile <- input$target_upload
     if (is.null(inFile))
       return(NULL)
     DT::datatable(
        if (input$choiceonTime != "All") {
          DataOntimeF<-DataOntime[DataOntime$OnTime == input$choiceonTime,]
        }
        else{
          DataOntimeF<-DataOntime
        })%>% formatStyle("OnTime", backgroundColor = styleEqual("0%", "red")) %>% 
     formatStyle("OnTime", backgroundColor = styleEqual("100%", "lightgreen"))
   
   })
  
  #Seleccionar datos tabla
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    
    Ordenes_volumen <<-  read_excel(inFile$datapath, sheet = "Ordenes_volumen")
    Tiempo_de_entrega <<-  read_excel(inFile$datapath, sheet = "Tiempo_de_entrega")
    
   
    FillRate<-Ordenes_volumen$`Fill rate`
    QtySolicitada<-Ordenes_volumen$`Sum Cantidad Original`
    QtyRecibida<- Ordenes_volumen$`Sum Cantidad Recibida Oracle`
    
    
    
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
    
    Ordenes_volumen$`Fill rate`<- QtyFillRate
    d1<-Ordenes_volumen[,1:8]
    d2<-select(Tiempo_de_entrega,"OrdendeCompra",OnTime,"Fecha Promesa Orignal","Fecha Recepción Oracle")
    d3<-left_join(d1, distinct(d2), by="OrdendeCompra")
    
    ListaRates<<- as.list(sqldf("SELECT `Fill rate` FROM Ordenes_volumen where `Fill rate` is not Null group by `Fill rate` "))
   
    
    colnames(d3)[which(names(d3) == "Fecha Recepción Oracle")]<-"Fecha Recepción"
    colnames(d3)[which(names(d3) == "Fecha Promesa Orignal")]<- "Fecha compromiso"
    colnames(d3)[which(names(d3) == "Sum Cantidad Original")]<- "Qtycompromiso"
    colnames(d3)[which(names(d3) == "OrdendeCompra")]<- "#Orden"
   
    d3$"Fecha Recepción"<-as.Date(d3$"Fecha Recepción",format="%d-%b-%y")
    d3$"Fecha compromiso"<-as.Date(d3$"Fecha compromiso",format="%d-%b-%y")
    
    DataOntime<<- d3
  
  })



}

  
  



  


  
 
  
# Run the application 
shinyApp(ui = ui, server = server)
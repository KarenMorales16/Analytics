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

genders = list()
corridas_ano = data.frame()
Datatime= data.frame()
Datatimes= data.frame()
Mes_f= data.frame()
Outoftime = data.frame()
Outoftime1= data.frame()
Outoftime2= data.frame()
Outoftime3= data.frame()
Outoftime4= data.frame()
Outoftime5= data.frame()
Outoftime6= data.frame()
BssRequired =  0
TssRequired =  0
THTRequired =  0
AFARequired =  0
BRKRequired =  0
# Define UI
ui <- dashboardPage(

  dashboardHeader(title = h4("NPI Overview Dashboard")),
  dashboardSidebar(  
    
    fluidPage(
      
      fileInput('target_upload', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv')
      ),  
      uiOutput('Lista')
      ,uiOutput( 'Boxes')
    )
  ),
  
  dashboardBody( 
    fluidRow(
      tabBox(
        title = "Desgloce de tiempos",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "100%", width = 800,
        tabPanel("NPIP Builds alloted time ",  plotlyOutput("graph") ,DT::dataTableOutput("sample_table") ),
        tabPanel("Montly overview", plotlyOutput("graph1") , DT::dataTableOutput("sample_table1")),
        tabPanel("Out of time",plotlyOutput("graph2"),DT::dataTableOutput("sample_table2"))
        
      )),uiOutput('value')
   
   
  )
  
  
)



  # Define server logic
server <- shinyServer(function(input, output,  session) {
  #Imprimir tabla
  output$sample_table<- DT::renderDataTable({
    
    df <- df_products_upload()
    
    DT::datatable(df)
    
  })
  
  output$sample_table1<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(Mes_f)
    
  })
  
  output$sample_table2<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(Outoftime)
    
  })
  
  #Seleccionar datos tabla
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
    return(NULL)

    df <- read.csv(inFile$datapath, header = TRUE)
    'return(df)'
    
    MesF<-df$Mes.Fiscal
    MesF[grepl("N/A", MesF)] <- NA
    MesF[MesF ==""] <- NA
    
    
    
    Bss <-df$BSS..
    Bss[grepl("N/A", Bss)] <- NA
    Bss[Bss ==""] <- NA
    
    
    Tss <-df$TSS..
    Tss[grepl("N/A", Tss)] <- NA
    Tss[Tss ==""] <- NA
    
    Edge <-df$Edge.Bonding..
    Edge[grepl("N/A", Edge)] <- NA
    Edge[Edge ==""] <- NA
    
    THT <-df$THT..
    THT[grepl("N/A", THT)] <- NA
    THT[THT ==""] <- NA
    
    
    
    Thic <-df$Thick.CC..
    Thic[grepl("N/A", Thic)] <- NA
    Thic[Thic ==""] <- NA
    
    
    AFA <-df$AFA..
    AFA[grepl("N/A", AFA)] <- NA
    AFA[AFA ==""] <- NA
    
    
    BRK <-df$Braket..
    BRK[grepl("N/A", BRK)] <- NA
    BRK[BRK ==""] <- NA
    
    Phase <-df$Phase
    Phase[grepl("N/A", Phase)] <- NA
    Phase[Phase ==""] <- NA
  
  
    
    
    data =  data.frame(  Mes_Fiscal = MesF, Bss = Bss , Tss = Tss,Edge=Edge, THT= THT,Thic= Thic,  AFA= AFA, BRK= BRK, Phase= Phase )
     
    Datatime<- data.frame(StatusBss  = (df$BSS...Hrs..Required..time >= df$BSS..Hrs..Real.used.time),
                          StatusTss=   (df$TSS...Hrs..Required..time >=  df$TSS..Hrs..Real.used.time),
                          StatusEdge = (df$Edge.Bonding...Hrs..Required..time >= df$Edge.Bonding...Hrs..Real.used.time),
                          StatusTHT=   (df$THT...Hrs..Required..time >= df$THT..Hrs..Real.used.time),
                          StatusThic=  (df$Thick.CC...Hrs..Required..time >= df$Thick.CC..Hrs..Real.used.time),
                          StatusAFA=   (df$AFA...Hrs..Required..time >=  df$AFA..Hrs..Real.used.time),
                          StatusBRK=   (df$Braket...Hrs..Required..time >= df$Braket..Hrs..Real.used.time),
                          Mes_Fiscal = MesF, Phase=Phase)
    
    
    Datatime$StatusBss[is.na(data$Bss)] <- 'NA'
    Datatime$StatusTss[is.na(data$Tss)] <- 'NA'
    Datatime$StatusEdge[is.na(data$Edge)] <- 'NA'
    Datatime$StatusTHT[is.na(data$THT)] <- 'NA'
    Datatime$StatusThic[is.na(data$Thic)] <- 'NA'
    Datatime$StatusAFA[is.na(data$AFA)] <- 'NA'
    Datatime$StatusBRK[is.na(data$BRK)] <- 'NA'
    
    
    
    Datatimes <- data.frame(sqldf("SELECT Mes_Fiscal,StatusBss as Estatus, count(StatusBss) as CantidadBss FROM Datatime wHERE Mes_Fiscal is not null GROUP BY StatusBss,Mes_Fiscal order by Mes_Fiscal ASC"))
    Datatimes <-  Datatimes %>%data.frame(sqldf("SELECT  count(StatusTss) as CantidadTss FROM Datatime wHERE Mes_Fiscal is not null GROUP BY StatusTss,Mes_Fiscal order by Mes_Fiscal ASC"))
    Datatimes <-  Datatimes %>%data.frame(sqldf("SELECT  count(StatusTHT) as CantidadTHT FROM Datatime wHERE Mes_Fiscal is not null GROUP BY StatusTHT,Mes_Fiscal order by Mes_Fiscal ASC"))
    Datatimes <-  Datatimes %>%data.frame(sqldf("SELECT  count(StatusAFA) as CantidadAFA FROM Datatime wHERE Mes_Fiscal is not null GROUP BY StatusAFA,Mes_Fiscal order by Mes_Fiscal ASC"))
    Datatimes <-  Datatimes %>%data.frame(sqldf("SELECT  count(StatusBRK) as CantidadBRK FROM Datatime wHERE Mes_Fiscal is not null GROUP BY StatusBRK,Mes_Fiscal order by Mes_Fiscal ASC"))
    
    Mes_f <<- data.frame(sqldf(paste("SELECT  * FROM Datatimes WHERE Mes_Fiscal = '",input$choice,"'    GROUP BY  Estatus,Mes_Fiscal order by Mes_Fiscal ASC",sep="")))
    Mes_f$Estatus[(Mes_f$Estatus == 'FALSE')] <<- 'Out of Time'
    Mes_f$Estatus[(Mes_f$Estatus == 'NA')] <<- 'N/A'
    Mes_f$Estatus[(Mes_f$Estatus == 'TRUE')] <<- 'In time'
    
    'Tercera Grafica'
    Outoftime1<<-data.frame(sqldf(paste("SELECT Mes_Fiscal, Phase,count(StatusBss) as CantidadBss FROM Datatime WHERE StatusBss = 'FALSE' and Mes_Fiscal= '",input$choice,"' GROUP BY  Phase, Mes_Fiscal order by Mes_Fiscal ASC",sep = "")))
    Outoftime2<<-data.frame(sqldf(paste("SELECT Mes_Fiscal, Phase,count(StatusTss) as CantidadTss FROM Datatime WHERE StatusTss = 'FALSE' and Mes_Fiscal= '",input$choice,"' GROUP BY  Phase, Mes_Fiscal order by Mes_Fiscal ASC",sep = "")))
    Outoftime3<<-data.frame(sqldf(paste("SELECT Mes_Fiscal, Phase,count(StatusTHT) as CantidadTHT FROM Datatime WHERE StatusTHT = 'FALSE' and Mes_Fiscal= '",input$choice,"' GROUP BY  Phase, Mes_Fiscal order by Mes_Fiscal ASC",sep = "")))
    Outoftime4<<-data.frame(sqldf(paste("SELECT Mes_Fiscal, Phase,count(StatusAFA) as CantidadAFA FROM Datatime WHERE StatusAFA = 'FALSE' and Mes_Fiscal= '",input$choice,"' GROUP BY  Phase, Mes_Fiscal order by Mes_Fiscal ASC",sep = "")))
    Outoftime5<<-data.frame(sqldf(paste("SELECT Mes_Fiscal, Phase,count(StatusBRK) as CantidadBRK FROM Datatime WHERE StatusBRK = 'FALSE' and Mes_Fiscal= '",input$choice,"' GROUP BY  Phase, Mes_Fiscal order by Mes_Fiscal ASC",sep = "")))
    
    Outoftime <<-merge(Outoftime1, merge(Outoftime2,merge(Outoftime3, merge(Outoftime4, Outoftime5, all = TRUE), all = TRUE) , all = TRUE), all = TRUE) # Equivalente
    'Cantidad de Corridas'
    
    Outoftime$CantidadBss[is.na(Outoftime$CantidadBss)] <<-'N/A'
    Outoftime$CantidadTss[is.na(Outoftime$CantidadTss)] <<-'N/A'
    Outoftime$CantidadTHT[is.na(Outoftime$CantidadTHT)] <<-'N/A'
    Outoftime$CantidadAFA[is.na(Outoftime$CantidadAFA)] <<-'N/A'
    Outoftime$CantidadBRK[is.na(Outoftime$CantidadBRK)] <<-'N/A'
  
    
    BssRequired <<-(sqldf(paste("SELECT  count(StatusBss)  FROM Datatime wHERE  Mes_Fiscal= ' ",input$choice," ' ", sep= "")))
    
    genders  <<- as.list(sqldf("SELECT  Mes_Fiscal FROM data where Mes_Fiscal is not Null group by Mes_Fiscal "))
    genders  <<- do.call(c, list('All',genders))
    
    if(input$choice =="All"){
    corridas_ano <<- data.frame(sqldf("SELECT Mes_Fiscal, count(Bss) as Corridas_BSS,count(Tss) AS Corridas_TSS, count(Edge) AS Corridas_Edge,count(THT) AS Corridas_THT,count(Thic) AS Corridas_THICKCC, count(AFA) AS Corridas_AFA, count(BRK) AS Corridas_BRACKET   FROM data where Mes_Fiscal is not Null  GROUP BY Mes_Fiscal"))
    }
    else{
    corridas_ano <<-  data.frame(sqldf(paste("SELECT Mes_Fiscal, count(Bss) as Corridas_BSS,count(Tss) AS Corridas_TSS, count(Edge) AS Corridas_Edge,count(THT) AS Corridas_THT,count(Thic) AS Corridas_THICKCC, count(AFA) AS Corridas_AFA, count(BRK) AS Corridas_BRACKET   FROM data where Mes_Fiscal is not Null And Mes_Fiscal ='",input$choice,"' GROUP BY Mes_Fiscal", sep="")))
    }
    
 

  })
  #Imprimir lista de ListBox
  output$Lista <- renderUI({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
      selectInput("choice", label = h5("Mes Fiscal"), 
                  choices = genders, 
                  selected = 1)
      
    
  })
  
  output$Boxes <- renderUI({
    df <- df_products_upload()
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    fluidRow(
      box(
        title = "BSS", width = 4, solidHeader = TRUE, status = "primary", BssRequired
      ),
      box(
        title = "TSS", width = 4, solidHeader = TRUE, status = "primary",TssRequired
       
      ),
      box(
        title = "THT", width = 4, solidHeader = TRUE, status = "primary",THTRequired
        
      ),
      box(
        title = "AFA", width = 4, solidHeader = TRUE, status = "primary",AFARequired
        
      ),
      box(
        title = "BRK", width = 4, solidHeader = TRUE, status = "primary",BRKRequired
        
      )
      
    )
      
  })
  
  #Imprimir valor Seleccionado
  #output$value <- renderPrint({ input$choice })  
  #Grafica Cantidad corridas
  output$graph <- renderPlotly({
    'Cantidad de Corridas'
     df_products_upload()
    inFile <- input$choice
    if (is.null(inFile))
      return(NULL)
    dt <- corridas_ano
    choice<- input$choice
    fig <- corridas_ano %>% plot_ly()
    fig <- fig %>% add_trace(x =  dt$Mes_Fiscal , y = ~dt$Corridas_BSS, type = 'bar', name='BSS',
                             text = ~dt$Corridas_BSS, textposition = 'auto',
                             marker = list(color = 'rgb(246, 232, 54)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))
    
    fig <- fig %>% add_trace(x = dt$Mes_Fiscal , y = ~dt$Corridas_TSS, type = 'bar', name='TSS',
                             text = ~dt$Corridas_TSS, textposition = 'auto',
                             marker = list(color = 'rgb(159, 218, 58)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    fig <- fig %>% add_trace(x =dt$Mes_Fiscal   , y = ~dt$Corridas_Edge, type = 'bar', name='Edge Bonding',
                             text = ~dt$Corridas_Edge, textposition = 'auto',
                             marker = list(color = 'rgb(74, 193, 109)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    fig <- fig %>% add_trace(x = dt$Mes_Fiscal , y = ~dt$Corridas_THT, type = 'bar', name='THT',
                             text = ~Corridas_THT, textposition = 'auto',
                             marker = list(color = 'rgb(31, 161, 135)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))
    
    fig <- fig %>% add_trace(x = dt$Mes_Fiscal  , y = ~dt$Corridas_THICKCC, type = 'bar', name='Thick CC',
                             text = ~dt$Corridas_THICKCC, textposition = 'auto',
                             marker = list(color = 'rgb(58,200,225)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))
    
    fig <- fig %>% add_trace(x = dt$Mes_Fiscal  , y = ~dt$Corridas_AFA, type = 'bar', name='AFA',
                             text = ~dt$Corridas_AFA, textposition = 'auto',
                             marker = list(color = 'rgb(54, 92, 141)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))
    
    fig <- fig %>% add_trace(x = dt$Mes_Fiscal  , y = ~dt$Corridas_BRACKET, type = 'bar', name='Braket',
                             text = ~dt$Corridas_BRACKET, textposition = 'auto',
                             marker = list(color = 'rgb(70, 51, 126)',
                                           line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    fig <- fig %>% layout(title = "NPIP Builds alloted time attainment",
                          barmode = 'group',
                          xaxis = list(title = "Meses"),
                          yaxis = list(title = "Etapa"))
    
    fig
  })
  
  output$graph1 <- renderPlotly({
    'Cantidad de Corridas'
    df_products_upload()
    inFile <- input$choice
    if (is.null(inFile))
      return(NULL)
    'Cantidad de Corridas'
    GraphicsTime <- Mes_f %>% plot_ly()
    GraphicsTime <- GraphicsTime %>% add_trace(x = ~Estatus, y = ~CantidadBss,  type = 'bar', name='BSS',
                                               text = ~CantidadBss, textposition = 'auto',
                                               marker = list(color = 'rgb(246, 232, 54)',
                                                             line = list(color = 'rgb(23,23,23)', width = 0)))
    
    GraphicsTime <- GraphicsTime %>% add_trace(x = ~Estatus, y = ~CantidadTss, type = 'bar', name='TSS',
                                               text = ~CantidadTss, textposition = 'auto',
                                               marker = list(color = 'rgb(159, 218, 58)',
                                                             line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    GraphicsTime <- GraphicsTime %>% add_trace(x = ~Estatus, y = ~CantidadTHT, type = 'bar', name='THT',
                                               text = ~CantidadTHT, textposition = 'auto',
                                               marker = list(color = 'rgb(74, 193, 109)',
                                                             line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    GraphicsTime <- GraphicsTime %>% add_trace(x = ~Estatus, y = ~CantidadAFA, type = 'bar', name='AFA',
                                               text = ~CantidadAFA, textposition = 'auto',
                                               marker = list(color = 'rgb(31, 161, 135)',
                                                             line = list(color = 'rgb(23,23,23)', width = 0)))
    
    GraphicsTime <- GraphicsTime %>% add_trace(x = ~Estatus, y = ~CantidadBRK, type = 'bar', name='BRK',
                                               text = ~CantidadBRK, textposition = 'auto',
                                               marker = list(color = 'rgb(58,200,225)',
                                                             line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    GraphicsTime <- GraphicsTime %>% layout(title = "Montly overview",
                                            barmode = 'group',
                                            xaxis = list(title = inFile),
                                            yaxis = list(title = "Etapa"))
    
    GraphicsTime
    
  })
  
  output$graph2 <- renderPlotly({
    'Cantidad de Corridas'
    df_products_upload()
    inFile <- input$choice
    if (is.null(inFile))
      return(NULL)
    'Cantidad de Corridas'
    GraphicsOutoftime <-  plot_ly()
    GraphicsOutoftime <- GraphicsOutoftime %>% add_trace(x = ~Outoftime1$Phase, y = ~Outoftime1$CantidadBss,  type = 'bar', name='BSS',
                                                         text = ~Outoftime1$CantidadBss, textposition = 'auto',
                                                         marker = list(color = 'rgb(246, 232, 54)',
                                                                       line = list(color = 'rgb(23,23,23)', width = 0)))
    
    GraphicsOutoftime <- GraphicsOutoftime %>% add_trace(x = ~Outoftime2$Phase, y = ~Outoftime2$CantidadTss, type = 'bar', name='Tss',
                                                         text = ~Outoftime2$CantidadTss, textposition = 'auto',
                                                         marker = list(color = 'rgb(58,200,225)',
                                                                       line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    GraphicsOutoftime <- GraphicsOutoftime %>% add_trace(x = ~Outoftime3$Phase, y = ~Outoftime3$CantidadTHT, type = 'bar', name='THT',
                                                         text = ~Outoftime3$CantidadTHT, textposition = 'auto',
                                                         marker = list(color = 'rgb(74, 193, 109)',
                                                                       line = list(color = 'rgb(23,23,23)', width = 0)))
    
    GraphicsOutoftime <- GraphicsOutoftime %>% add_trace(x = ~Outoftime4$Phase, y = ~Outoftime4$CantidadAFA, type = 'bar', name='AFA',
                                                         text = ~Outoftime4$CantidadAFA, textposition = 'auto',
                                                         marker = list(color = 'rgb(31, 161, 135)',
                                                                       line = list(color = 'rgb(23,23,23)', width = 0)))
    
    GraphicsOutoftime <- GraphicsOutoftime %>% add_trace(x = ~Outoftime5$Phase, y = ~Outoftime5$CantidadBRK, type = 'bar', name='BRK',
                                                         text = ~Outoftime5$CantidadBRK, textposition = 'auto',
                                                         marker = list(color = 'rgb(70, 51, 126)',
                                                                       line = list(color = 'rgb(23,23,23)', width = 0)))
    
    
    
    GraphicsOutoftime <- GraphicsOutoftime %>% layout(title = "Phase de corridas Fuera de Tiempo",
                                                      barmode = 'group',
                                                      xaxis = list(title = inFile ),
                                                      yaxis = list(title = "Etapa"))
    
    GraphicsOutoftime
    
  })
  

 
  
  #Fin server 
})

# Run the application 
shinyApp(ui = ui, server = server)
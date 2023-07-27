##install.packages("mapview")
library(sf)
library(ggplot2)
library(mapview)
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


Ordenes_volumen <- read_excel("C:/Users/kmorales/Downloads/Prueba de Indicadores-Mayo2023.xlsx", sheet = "Ordenes_volumen")
Tiempo_de_entrega <- read_excel("C:/Users/kmorales/Downloads/Prueba de Indicadores-Mayo2023.xlsx", sheet = "Tiempo_de_entrega")


FillRate<-Ordenes_volumen$`Fill rate`
QtySolicitada<-Ordenes_volumen$`Sum Cantidad Original`
QtyRecibida<- Ordenes_volumen$`Sum Cantidad Recibida Oracle`

#Renombrar columas

colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == '# orden de compra')] <-"OrdendeCompra"
colnames(Tiempo_de_entrega)[which(names(Tiempo_de_entrega) == '# Orden de compra')] <-"OrdendeCompra"
colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == 'Sum Cantidad Recibida Oracle')]<-"QtyRecibida"
colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == 'Artículo')]<-"Articulo"
colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == 'Descripción Artículo')]<-"Descripcion"


# Calcular FillRate
QtyFillRate <-(QtyRecibida/QtySolicitada)

# Calcular Ontime
Tiempo_de_entrega$OnTime<-(Tiempo_de_entrega$`Fecha Promesa Orignal`)>=(Tiempo_de_entrega$`Fecha Recepción Oracle`)
Tiempo_de_entrega$OnTime[Tiempo_de_entrega$OnTime ==TRUE] <- "100%"
Tiempo_de_entrega$OnTime[Tiempo_de_entrega$OnTime ==FALSE] <- "0%"

Ordenes_volumen$`Fill rate`<-  as.numeric(sprintf("%.1f",QtyFillRate))



d1<-Ordenes_volumen[,1:8]
d2<-select(Tiempo_de_entrega,"OrdendeCompra",OnTime,"Fecha Promesa Orignal","Fecha Recepción Oracle")
d3<-left_join(d1, distinct(d2), by="OrdendeCompra")

colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Sum Cantidad Recibida Oracle")]<-"QtyRecibida"
colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Artículo")]<-"Articulo"
colnames(Ordenes_volumen)[which(names(Ordenes_volumen) == "Descripción Artículo")]<-"Descripcion"


TopProVolumen<-
  sqldf("SELECT Articulo,Descripcion,Proveedor,SUM(QtyRecibida) as VolumenTotal FROM Ordenes_volumen GROUP BY Proveedor,Articulo  ORDER by VolumenTotal DESC")

graphTopProVolumen<-
  sqldf("SELECT Articulo,Descripcion,Proveedor,SUM(QtyRecibida) as VolumenTotal FROM Ordenes_volumen GROUP BY Proveedor  ORDER by VolumenTotal DESC")

colnames(d3)[which(names(d3) == 'Fecha Recepción Oracle')]<-"Fecha Recepción"
colnames(d3)[which(names(d3) == 'Fecha Promesa Orignal')]<- "Fecha compromiso"
colnames(d3)[which(names(d3) == 'Sum Cantidad Original')]<- "Qtycompromiso"
colnames(d3)[which(names(d3) == 'OrdendeCompra')]<- "#Orden"

d3$"Fecha Recepción"<-as.Date(d3$"Fecha Recepción",format="%d-%b-%y")
d3$"Fecha compromiso"<-as.Date(d3$"Fecha compromiso",format="%d-%b-%y")

Ordenes_volumen$CEDIS

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

# Descarga el archivo zip de github
download.file(url = "https://github.com/prestevez/covid-19-mx-map/raw/master/datos_covid/01_32_mun.zip",
              destfile = "datos_covid/01_32_mun.zip")

# Extrae el archivo zip en la carpeta datos_covid
unzip("datos_covid/01_32_mun.zip",
      exdir = "datos_covid/")

# Carga el archivo shapefile a R
ctes <- st_read("datos_covid/01_32_mun.shp")

datos_mun <- read_csv("https://raw.githubusercontent.com/prestevez/covid-19-mx-map/master/datos_covid/Casos_Diarios_Estado_Nacional_Confirmados.csv")


CompraVolumen<-sqldf("SELECT cve_ent,Cuidad,SUM(QtyRecibida) as poblacion FROM DataOntime GROUP BY Cuidad  ORDER by poblacion DESC")

#METODO PARA ELIMINAR VALORES NULOS FE
delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

# vector con los puntos de corte
brks <- c(0,1000,6000,11000,21000,51000,91000,101000,121000,
          max(CompraVolumen$poblacion, na.rm = TRUE))

mex_map_covid <- ctes %>%
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

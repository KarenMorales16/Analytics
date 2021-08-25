install.packages("tidyverse")  
library(tidyverse)
install.packages("readxl")
library(readxl)

# Datos SMT & ASE 
DataSMT <-  read_excel("2.xlsx", sheet ="WK 22", range = "A12:B587") 
# Datos SMT & ASE 
DataSMT1 <-  read_excel("2.xlsx", sheet ="WK 22", range = "F12:F587") 
DataSMT2 <-  read_excel("2.xlsx", sheet ="WK 22", range = "AJ12:AJ587") 

DataSMT <-  data.frame(DataSMT, DataSMT1,DataSMT2 )



DataSMT[,2:2][DataSMT[,2:2] =="AMPLIFIER"] <- NA
LineDataASE<- DataSMT[!is.na( DataSMT[,2:2]),]


data <-  data.frame(Nombre=LineDataASE[,1:1], PN=LineDataASE[,1:1])


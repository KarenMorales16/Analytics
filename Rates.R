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




#Agregamos nombres a los columnas
colnames(datosSMT)= unlist (c('SMTProduct','SMTPN','SMTLado','SMTRate','SMTLinea'))

#Eliminamos los datos irrelevantes en este caso Amplificadores ya que no tiene
#valor significativo para nuestra data
datosSMT[,2:2][datosSMT[,2:2] =="AMPLIFIER"] <- NA
datosSMT<- datosSMT[!is.na( datosSMT[,2:2]),]

#Los datos en NA los colocamos en vacio para que nos permitan trabajar con ellos
# y no sean descartados
datosSMT[,1:1][is.na(datosSMT[,1:1])] <- ""

#Agregamos La linea
datosSMT$SMTLinea <-data.frame(SMTLinea=ifelse(datosSMT$SMTProduct=="SMT", paste(datosSMT$SMTProduct,datosSMT$SMTPN, sep =" "),NA))
SMTLinea <-c(ifelse(datosSMT$SMTProduct=="SMT", paste(datosSMT$SMTProduct,datosSMT$SMTPN, sep =" "),NA))
LineasSMT<- (datosSMT$SMTLinea[!is.na( datosSMT$SMTLinea),])


y=0
for( i in 1:length(SMTLinea)){
  if(!is.na(SMTLinea[i])){
    SMTLinea[i]=LineasSMT[y]
  }
  else(is.na(SMTLinea[i]))
  {
    y=y+1
  }
    
}
 
SMTLinea<- data.frame(LineasSMT)




library(datos)
paises <- datos ::paises
names(paises$esperanza_de_vida, paises$pib_per_capita)= c("Lifeexpectancy","","",FertilityRate")


colnames()

#Debe generar un diagrama de dispersi�n (scatter-plot) que muestre las estad�sticas
  #esperanza de vida = esperanza_de vida
de esperanza de vida ( Life expectancy - eje y) y tasa de fertilidad (Fertility Rate -eje
                                                                      x) por pa�s (Country).

#El diagrama de dispersi�n tambi�n debe clasificarse por pa�ses Regiones (Country
                                                                         Regions).

#Se le han proporcionado datos durante 2 a�os: 1960 y 2013 y se le exige que
produzca una visualizaci�n para cada uno de estos a�os.
Algunos datos se han proporcionado en un archivo CVS, algunos en vectores R. El
archivo CVS contiene datos combinados de ambos a�os. Toda manipulaci�n de datos
debe realizarse en R (No en Excel) porque este proyecto puede ser auditado en una
etapa posterior.
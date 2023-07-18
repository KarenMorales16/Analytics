#install.packages("RODBC")
library(RODBC) 
cn  <- odbcDriverConnect(connection="Driver={SQL Server};server=LAPTOP-BIT1OKR8;database=Analytics;trusted_connection=yes;")
#CREAR TABLA
sqlQuery(cn, 'CREATE TABLE Usuario(id int identity(1,1) PRIMARY KEY,
               Nombre VARCHAR(50),
               Nacionalidad VARCHAR(20),
               FechaNac date)')


#AÑADIR DIRECCIÓN
sqlQuery(cn,'ALTER TABLE Usuario ADD Direccion Varchar(30)')


#MODIFICAR UNA COLUMNA
sqlQuery(cn,"ALTER TABLE Usuario ALTER COLUMN Nombre VARCHAR(60)")

#CREAR TABLA CARRERA
sqlQuery(cn," CREATE TABLE Carrera(IdCarrera varchar(5) PRIMARY KEY
         , Carrera VARCHAR(30))")

#AÑADIR ELEMENTOS A CARRERA
sqlQuery(cn,"INSERT Carrera (IdCarrera,Carrera) values ('ISC','Ingenieria en sistemas')")

sqlQuery(cn,"INSERT Carrera(IdCarrera,Carrera) Values ('IND','Ingenieria Industrial')")



#AGREGAR IdCarrera en Usuario
sqlQuery(cn,"ALTER TABLE Usuario ADD IdCarrera VARCHAR(5)")

#AGREGAR LLAVE FOREANA IdCarrera a Usuario
sqlQuery(cn,"ALTER TABLE Usuario  ADD CONSTRAINT FK_IdCarera FOREIGN KEY (IdCarrera)REFERENCES Carrera(IdCarrera)")

#DESPLEGAR TODOS LOS ELEMETOS DE CARRERA
sqlQuery(cn,"SELECT IdCarrera FROM Carrera")

#AÑADIR REGISTROS
sqlQuery(cn,"INSERT INTO Usuario VALUES('Karen Morales', 'Mexicana','19/Oct/1997','Tijuana', 'ISC')")
sqlQuery(cn,"INSERT INTO Usuario VALUES('Marry Morales', 'Colombia','10/10/1999','Brqlla', 'IND')")

 #PAQUETE SQLDF sirve para ejecutar sentencias SQL en marcos de datos R, optimizado para mayor comodidad.

 ##install.packages("sqldf")
 ## install.packages("datos", dependencies = T)
 require(sqldf)
 library(datos)
 paises <- datos ::paises

 View(paises)

 # Comodín *
 sqldf("SELECT * FROM paises")
 
 #SELECT variable1, variable2 FROM data
 sqldf(("SELECT continente, pais from paises"))
 
 # LIMIT #.
 sqldf("SELECT Continente, pais from paises LIMIT 5")
 
 # SUM()
 sqldf("SELECT SUM(pib_per_capita) AS 'SUMA'  from paises where")
 
 # AVG ()
 sqldf("SELECT AVG(pib_per_capita) AS 'PROMEDIO' from paises where continente='Asia' ")
 
 # ver cuantos COUNT
sqldf("SELECT COUNT(CONTINENTE) AS NUMCONTINENTE FROM paises where Continente='Asia'")
  
 # sqldf ("SELECT COUNT(species), species from iris GROUP BY species")
sqldf("SELECT CONTINENTE AS DIVERSIDADCONTINENTE FROM paises GROUP BY  continente")
 

 # DESC ASC
 sqldf("SELECT Continente as 'Nombre continente',UPPER(pais)  AS PAIS,SUM(pib_per_capita) 
       from paises GROUP BY continente")


 # upper,lower
 names(paises)
 sqldf("SELECT UPPER(Continente) AS 'CONTINENTE',LOWER(pais) AS 'pais',ANIO AS 'AÑO',poblacion rom paises limit 5 ")
 
 
 # WHERE < > != <> ==
 sqldf("SELECT Continente, pais Anio from paises where anio>2006 ")
#BETWEEN 
 sqldf("SELECT * from paises where anio between 2006 and 2010 limit 5")
 
#TERMINE LIKE '%'
 sqldf("SELECT   pais from paises WHERE pais LIKE '%A' ")
 

 #INICIO LIKE 'A%'

 sqldf("SELECT   pais from paises WHERE pais LIKE 'A%'")
 
 # TINE LIKE '%%'
 sqldf("SELECT   pais from paises WHERE pais LIKE '%A%'")
 
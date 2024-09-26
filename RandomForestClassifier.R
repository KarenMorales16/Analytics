# Random Forest Classification

getwd()
setwd("/home/karen/Escritorio/Algoritmos")
getwd()

# Importing the dataset
dataset = read.csv('Datos.csv')

#Categorical data
dataset$Maintenance.equipment = factor(dataset$Maintenance.equipment, levels = c("A","B","C"), labels = c(1,2,3))

# Encoding the target feature as factor
dataset$Faults = factor(dataset$Faults, levels = c(0, 1))

# Dividir el conjunto de datos de entrenamiento y conjunto de test
#install.packages("CaTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Faults, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# Ajustar el Ramdom Forest con el conjunto de entrenamiento
# install.packages('randomForest')
library(randomForest)
set.seed(123)
#vector de caracteristicas menos la variable a predecir
#y variable a predecir
#numero arboles 10
# x=[1,1,1,99]
classifier = randomForest(x = training_set[-5],
                          y = training_set$Faults,
                          ntree =10)


# Predicion de los resuktados con el conjunto de testing
y_pred = predict(classifier, newdata = test_set[-5])
y_pred

# Matriz de confucion
cm = table(test_set[, 5], y_pred)
cm

# % correcto
100 * sum(diag(cm)) / sum(cm)



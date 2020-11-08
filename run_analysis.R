# 
# author: "Nelson Sánchez Sánchez"
# date: "7/11/2020"
#

library(dplyr)

##############################################################################
# PASO 1 - Obtener Datos
##############################################################################

## Descargar los datos

# Definir la ruta en donde se encuentra el archivo con los datos y asignar un nombre con el cuál quedará almacenado el archivo en una carpeta local.

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

# Descargar el archivo si este todavía no ha sido colocado en una carpeta local
if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}
# Descomprimir el archivo que contiene los datos si aún no se ha realizado
rutaDeDatos <- "UCI HAR Dataset"
if (!file.exists(rutaDeDatos)) {
  unzip(zipFile)
}
##############################################################################
# PASO 2 - Leer los datos
##############################################################################

#   Leer los datos de ENTRENAMIENTO
trainingSubjects <- read.table(file.path(rutaDeDatos, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(rutaDeDatos, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(rutaDeDatos, "train", "y_train.txt"))

#   Leer los datos de PRUEBA
testSubjects <- read.table(file.path(rutaDeDatos, "test", "subject_test.txt"))
testValues <- read.table(file.path(rutaDeDatos, "test", "X_test.txt"))
testActivity <- read.table(file.path(rutaDeDatos, "test", "y_test.txt"))
#   Leer la características (features), sin convertir los text labels a factores
features <- read.table(file.path(rutaDeDatos, "features.txt"), as.is = TRUE)
#   Leer las actividades
actividades <- read.table(file.path(rutaDeDatos, "activity_labels.txt"))
colnames(actividades) <- c("activityId", "activityLabel")

##############################################################################
# Paso 3 - Mezclar los datos de prueba y los datos de entrenamiento 
#          para crear un solo data set
##############################################################################

#   Concatenar las tablas individuales de datos y construir un solo data set
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

#   Eliminar de la memoria las tablas individuales para ahorrar espacio
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)
#   Asignar nombres a las columnas
colnames(humanActivity) <- c("subject", features[, 2], "activity")

##############################################################################
# Paso 4 - Extraer solo las medidas relacionadas con la media y la desviación 
#          estandar para cada medición
##############################################################################

#         Determinar qué columnas de data set se usarán tomando como base 
#         los nombres de las columnas y mantener solo los datos de estas
#         columnas para el análisis
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

##############################################################################
# Paso 5 - Usar nombres descriptivos para cada una de las actividades
#          dentro del data set reemplazar valores por factores con nombre
##############################################################################
humanActivity$activity <- factor(humanActivity$activity, 
  levels = actividades[, 1], labels = actividades[, 2])

##############################################################################
# Paso 6 - Etiquetar los data set con nombres de variables descriptivos
#          tomar los nombres de las columnas, quitar los caracteres especiales
##############################################################################
humanActivityCols <- colnames(humanActivity)
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
#          reemplazar las abreviaturas por nombres completos
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
#          corregir errores o palabras repetidas
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

#          usar nuevas etiquetas para los nombres de las columnas
colnames(humanActivity) <- humanActivityCols

##############################################################################
# Paso 6 - Crear un segundo e independiente tidy set con el promedio de cada 
#          variable para cada actividad y cada tema
#          agrupar por tema y actividad y resumir usando el promedio (mean)
##############################################################################
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))
##############################################################################
# Paso 7. Almacenar el resultado en el archivo "tidy_data.txt"
write.table(humanActivityMeans,"tidy_data.txt",row.names = FALSE,quote = FALSE)

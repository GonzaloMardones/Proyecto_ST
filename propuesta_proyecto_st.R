# Librerias
library(dplyr)
library(rio)
library(ggplot2)
library(descomponer)
library(dplyr)

library(forecast)
library(tseries)

df <- rio::import("/Users/macbookpro/Desktop/MAGISTER/Series_Tiempo/Proyecto/base_tasa_participacion.xlsx", sheet="Datos")
df$Fecha <- as.Date(paste0("01.", df$Periodo), format = "%d.%b.%Y")


# Serie de tiempo General
serie_tiempo_general <- ts(data = df[, -1], start = c(2010, 3), frequency = 12)
serie_tiempo <- ts(data = df$Tasa, start = c(2010, 3), frequency = 12)

# Asignar las fechas a ST Tasa
time(serie_tiempo) <- datos$Fecha
print(serie_tiempo)
plot(serie_tiempo, xlab = "Fecha", ylab = "Tasa", main = "Tasa de Participación")

# Serie de tiempo General
print(serie_tiempo_general)
plot(serie_tiempo_general)

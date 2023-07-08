library(dplyr)
library(rio)
library(ggplot2)
library(descomponer)
library(lubridate)
library(forecast)
library(tseries)

ruta <- "/Users/macbookpro/Desktop/MAGISTER/Series_Tiempo/Proyecto/"
tasa_participacion <- "base_tasa_participacion.xlsx"
ocupacion <- "ocupados_por_grupo_de_ocupacion.xlsx"
imacec <- "imacec.xlsx"

# Carga de datos (tasa participacion, ocupacion, imacec)
df_participacion <- rio::import(file.path(ruta, tasa_participacion), sheet="Datos")
df_ocupacion <- rio::import(file.path(ruta, ocupacion))
df_imacec <- rio::import(file.path(ruta, imacec))

#Parse Fecha Participacion - Ocupacion - Imacec
df_participacion$Fecha <- as.Date(paste0("01.", df_participacion$Periodo), format = "%d.%b.%Y")
df_participacion_filtrados <- subset(df_participacion, Fecha <= as.Date("2022-12-31"))
df_ocupacion_filtrados <- subset(df_ocupacion, Periodo <= as.Date("2022-12-31"))
df_imacec_filtrados <- subset(df_imacec, Fecha <= as.Date("2022-12-31"))


# Serie de tiempo General
serie_tiempo_general <- ts(data = df_participacion_filtrados[, -1], start = c(2010, 3), frequency = 12)

# Serie de tiempo participacion
serie_tiempo <- ts(data = df_participacion_filtrados$Tasa, start = c(2010, 3), frequency = 12)

# --------- DUMMIES ----------
# DUMMY - ABRIL 2020
#df_participacion_filtrados$dummy_ABR_2020 <- ifelse(df_participacion_filtrados$Fecha=='2020-04-01', 1, 0)


# Crear las columnas "mes" y "año"
df_participacion_filtrados <- df_participacion_filtrados %>%
  mutate(mes = match(month(Fecha, label = TRUE), month.abb),año = year(Fecha))

# Crear variables dummy para los meses
meses_dummy <- model.matrix(~ factor(df_participacion_filtrados$mes) - 1)

# Crear un dataframe con las variables exógenas
exogenas <- data.frame(meses_dummy)

exogenas <- exogenas %>%
  rename_with(~gsub("^factor\\.df_participacion_filtrados\\.mes\\.", "mes.", .), starts_with("factor.df_participacion_filtrados.mes."))


# Agregar variables macroeconómicas, ocupacion, imacec, al dataframe exogenas para crear modelo
modelo_datos <- cbind(serie_tiempo,
                      imacec = df_imacec_filtrados$`Imacec desestacionalizado (promedio móvil de 3 meses)`, 
                      ocupacion= df_ocupacion_filtrados$Total, 
                      exogenas
                      )
#                      df_participacion_filtrados$dummy_ABR_2020)

# Ajustar un modelo de regresión lineal
modelo_regresion <- lm(serie_tiempo ~ ., data = modelo_datos)
summary(modelo_regresion)

meses_dummy_imacec <- model.matrix(~ factor(df_imacec$Mes) - 1)
# Crear un dataframe para realizar predicciones
nuevas_exogenas <- data.frame(meses_dummy_imacec, 
                              imacec =  df_imacec$`Imacec desestacionalizado (promedio móvil de 3 meses)`, 
                              ocupacion= df_ocupacion$Total
                              ) 

nuevas_exogenas <- nuevas_exogenas %>%
  rename_with(~sub("^factor\\.df_imacec\\.Mes\\.", "exogenas.mes.", .))

#nuevas_exogenas$dummy_ABR_2020 <- ifelse(df_participacion_filtrados$Fecha=='2020-04-01', 1, 0)

nuevas_fechas <- seq(as.Date("2010-03-01"), as.Date("2023-05-01"), by = "month")
nuevos_datos <- data.frame(fecha = nuevas_fechas)

nuevos_datos$prediccion  <- predict(modelo_regresion, newdata = nuevas_exogenas)

ggplot() +
  geom_line(data = df_participacion_filtrados, aes(x = Fecha, y = Tasa), color = "blue", linetype = "solid", size = 1) +
  geom_line(data = nuevos_datos, aes(x = fecha, y = prediccion), color = "red", linetype = "dashed", size = 0.5) +
  labs(x = "Fecha", y = "Venta de Créditos", title = "Datos Originales y Predicciones del Modelo") +
  theme_minimal()


# Asignar las fechas a ST
print(serie_tiempo)
plot(serie_tiempo, xlab = "Fecha", ylab = "Tasa", main = "Tasa de Participación")

fit <- auto.arima(serie_tiempo)
fit
plot(fit)
plot(forecast(fit, h=24))

# Serie de tiempo General
print(serie_tiempo_general)
plot(serie_tiempo_general)

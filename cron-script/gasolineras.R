#!/usr/bin/env Rscript


# Cargar paquetes ---------------------------------------------------------

# install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, httr, readxl, xml2, jsonlite, janitor, leaflet, leaflet.extras)


# Obtener datos de un servicio web ------------------------------------------

url <- "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"
datos_json <- jsonlite::fromJSON(url)

# Comprobamos que los datos son correctos:
if (datos_json$ResultadoConsulta != "OK") {
  stop("Error al cargar los datos.")
}

# Seleccionamos sólo los datos:
df <- datos_json$ListaEESSPrecio

# Como es un dataframe, lo pasamos a tibble:
df <- as_tibble(df) 


# Limpieza de datos -------------------------------------------------------

# Quitamos acentos y espacios de los nombres de columnas
clean <- clean_names(df)

# El separador decimal es la coma en vez del punto
clean <- type_convert(clean, locale = locale(decimal_mark = ","))

# Añadir columna para gasolineras no abiertas 24h
clean <- mutate(clean, "abierto24h" = horario=="L-D: 24H")
clean <- mutate(clean, "fecha" = Sys.Date())

write.table(clean, file="temp.csv", sep=",", row.names = FALSE)
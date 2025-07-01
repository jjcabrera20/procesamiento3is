library(tidyverse)
library(readxl)
library(stringr)

datos_principal <- read_excel("datos_3is.xlsx", sheet = "Datos_principal", guess_max = 10000)
datos_personas <- read_excel("datos_3is.xlsx", sheet = "Datos_personas", guess_max = 10000)


datos_full <- left_join(datos_personas, datos_principal, by = "_parent_index")



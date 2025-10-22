# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerias
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(eph)
library(janitor)
library(tidyr)

######### LEER DATOS #########
Raw <- get_microdata(
  year = 2024,
  period = 1:4,
  type = "individual",
  vars = c("ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA" , "CH04", "P21", "PP04B_COD",
           "CH06", "ESTADO", "CAT_OCUP", "CAT_INAC", "INTENSI", "PP03J", "PP04D_COD")) %>%
  rbind(get_microdata(
    year = 2025,
    period = 1,
    type = "individual",
    vars = c("ANO4", "TRIMESTRE", "REGION", "AGLOMERADO", "PONDERA" , "CH04", "P21", "PP04B_COD",
             "CH06", "ESTADO", "CAT_OCUP", "CAT_INAC", "INTENSI", "PP03J", "PP04D_COD")))

write.csv(Raw, file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos_EPH.csv"))
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos_EPH.csv"))

# Todos los indicadores
Data <- Raw %>%
  mutate(ANO4 = as.character(ANO4), TRIMESTRE = as.character(TRIMESTRE)) %>%
  filter(AGLOMERADO == 23, CH06 >= 14) %>%
  organize_labels(type = "individual") %>%
  group_by(ANO4, TRIMESTRE, CH04) %>%
  summarise(Poblacion = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Subocupados = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]) + sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Tasa_Actividad = round(PEA/Poblacion * 100, 1),
            Tasa_Empleo = round(Ocupados/Poblacion * 100, 1),
            Tasa_Desocupacion = round(Desocupados/PEA * 100, 1),
            Tasa_Subocupacion = round(Subocupados/PEA * 100, 1)) %>%
  ungroup %>%
  mutate(CH04 = ifelse(CH04 == "1", "Varones", "Mujeres"),
         TRIMESTRE = str_sub(TRIMESTRE, 1,1)) %>%
  rename(Año = "ANO4", Trimestre = "TRIMESTRE", Género = "CH04")

# Todos los indicadores
Data1 <- Raw %>%
  mutate(ANO4 = as.character(ANO4), TRIMESTRE = as.character(TRIMESTRE)) %>%
  filter(AGLOMERADO == 23, CH06 >= 14) %>%
  organize_labels(type = "individual") %>%
  group_by(ANO4, TRIMESTRE, CH04) %>%
  summarise(Poblacion = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Subocupados = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]) + sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)])) %>%
  ungroup %>%
  mutate(CH04 = ifelse(CH04 == "1", "Varones", "Mujeres"),
         TRIMESTRE = str_sub(TRIMESTRE, 1,1)) %>%
  rename(Año = "ANO4", Trimestre = "TRIMESTRE", Género = "CH04") %>%
  pivot_longer(
    cols = c(Poblacion, Ocupados, Desocupados, PEA, Subocupados),
    names_to = "Indicador",
    values_to = "Valor") %>%
  filter(Indicador == "Subocupados")
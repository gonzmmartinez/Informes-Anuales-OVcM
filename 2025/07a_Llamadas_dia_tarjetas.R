# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Dia") %>%
  filter(Accion == "Llamadas", Tipo != "Abuso sexual")

Dia_lvl <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
Mes_lvl <- c("Junio", "Mayo", "Abril", "Marzo", "Febrero", "Enero")

Total_2025 <- Raw %>%
  filter(Año == 2025) %>%
  mutate(Dia = factor(Dia, levels = Dia_lvl)) %>%
  mutate(Mes = factor(Mes, levels= Mes_lvl)) %>%
  group_by(Año, Mes, Dia) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup %>%
  filter(Dia %in% c("Sábado", "Domingo")) %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad),
            Porcentaje = sum(Porcentaje)) %>%
  pull(Porcentaje)

Total_anterior <- Raw %>%
  filter(Año == 2024,
         Mes %in% c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")) %>%
  mutate(Dia = factor(Dia, levels = Dia_lvl)) %>%
  mutate(Mes = factor(Mes, levels= Mes_lvl)) %>%
  group_by(Año, Mes, Dia) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup %>%
  filter(Dia %in% c("Sábado", "Domingo")) %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad),
            Porcentaje = sum(Porcentaje)) %>%
  pull(Porcentaje)

# Grafico

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=4.5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=4.5)
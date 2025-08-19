# Limpiar todo
rm(list = ls())

# Librerias
library(openxlsx)
library (tidyverse)
library (ggforce)
library (ggplot2)
library (ggthemes)
library(devtools)
library(rgdal)
library(geogrid)
library(sf)
library(tmap)
library(ggspatial)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Salta_munic_shape/Salta_shape.shp"))

# Cargar datos
Data <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Mapa_poseen_area.csv")) %>%
  mutate(ID = as.character(ID)) %>%
  rename(in1 = "ID") %>%
  mutate(Area = ifelse(Area == "TRUE", "Con área", "Sin área"))

# Modificar datos
Mapa_Salta <- Mapa_Salta %>%
  left_join(Data, by="in1") %>%
  replace(is.na(.), "Sin dato")

# Colores
Colores <- c("Con área" = "#a5549c",
             "Sin área" = "#4ea6be",
             "Sin dato" = "grey")

# Grafico
grafico <- ggplot(Mapa_Salta) +
  geom_sf(color="black", aes(fill=Area)) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) + 
  labs(title=str_wrap("Municipios que poseen Área de Mujeres, Género y Diversidad", 70),
       subtitle=str_wrap("Año 2024. Provincia de Salta.",50)) +
  theme_void() +
  scale_fill_manual(values=Colores) +
  theme(legend.title=element_blank(),
        text=element_text(family="font"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.text = element_text(size=12, family="font"),
        plot.background = element_rect(fill="white", color=NA)) +
  guides(color = guide_legend(override.aes=list(shape = 16)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=9, height=6)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=9, height=6)
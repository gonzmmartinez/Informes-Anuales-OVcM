# Limpiar todo
rm(list = ls())

# Librerias
library(openxlsx)
library(tidyverse)
library(ggforce)
library(ggplot2)
library(ggthemes)
library(devtools)
library(rgdal)
library(geogrid)
library(sf)
library(tmap)
library(ggspatial)
library(ggpp)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Mapa_shape/Salta_deptos_shape/Salta_deptos.shp")) %>%
  mutate(etiqueta = case_when(etiqueta == "San Martín" ~ "General José de San Martín",
                              .default = etiqueta))

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa")

Poblacion <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                        sheet = "Poblacion")

Data <- Raw %>%
  filter(Año == 2025, Tipo %in% c("Género", "Familiar", "No penal")) %>%
  group_by(Año, Departamento) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  left_join(Poblacion, by=c("Año", "Departamento")) %>%
  rename(Cantidad = "Cantidad.x", Poblacion = "Cantidad.y") %>%
  mutate(Tasa = 100 * Cantidad / Poblacion) %>%
  group_by(Año) %>%
  arrange(desc(Tasa)) %>%
  mutate(Ord = row_number(),
         Dept_facet = factor(paste(Departamento, Año),
                             levels = paste(Departamento, Año)[order(Ord)])) %>%
  ungroup() %>%
  rename(etiqueta = "Departamento")

Mapa_Salta <- Mapa_Salta %>%
  left_join(Data, by="etiqueta") %>%
  mutate(Tasa = ifelse(is.na(Tasa), 0, Tasa))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- colorRampPalette(c("white", "#206170"))(3)

# Grafico
grafico <- ggplot(Mapa_Salta) +
  geom_sf(color="black", aes(fill=Tasa), linewidth=0.5) +
  theme_void() +
  guides(fill = guide_colorbar(theme = theme(legend.frame = element_rect(colour = "black")))) +
  scale_fill_gradient2(low="white", high="#ff621d", midpoint=0.9,
                       limits=c(min(Data$Tasa),max(Data$Tasa)), na.value="grey80") +
  theme(legend.title = element_blank(),
        text = element_text(family="font_sans"),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(family="font_sans", size=20),
        legend.key.spacing.y = unit(0, "cm"))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=6)


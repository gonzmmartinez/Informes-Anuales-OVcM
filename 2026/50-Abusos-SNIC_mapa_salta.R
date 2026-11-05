# Limpiar todo
rm(list = ls())

# Librerias
library(openxlsx)
library(tidyverse)
library(ggforce)
library(ggthemes)
library(devtools)
library(geogrid)
library(sf)
library(tmap)
library(ggspatial)
library(ggpp)
library(googlesheets4)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Mapa_shape/Salta_deptos_shape/Salta_deptos.shp")) %>%
  mutate(etiqueta = case_when(etiqueta == "San Martín" ~ "General José de San Martín",
                              .default = etiqueta))

# Cargar datos
Raw <- read.csv2(
  file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
              "/Datos/snic-departamentos-anual.csv"))
Poblacion <- read.csv(
  file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
              "/Datos/Poblacion_Departamentos_Salta.csv")) %>%
  filter(Año == 2025)

# Modificar datos
Data <- Raw %>%
  filter(provincia_id == 66,
         codigo_delito_snic_id %in% c("10", "11", "11_1", "11_2", "11_3", "11_4", "11_5"),
         anio == 2025) %>%
  group_by(departamento_nombre) %>%
  summarise(Cantidad = sum(cantidad_victimas), .groups = "drop") %>%
  filter(departamento_nombre != "Departamento sin determinar") %>%
  mutate(departamento_nombre = recode(
    departamento_nombre,
    "General Gúemes" = "General Güemes")) %>%
  left_join(
    Poblacion,
    by = c("departamento_nombre" = "Departamento")
  ) %>%
  mutate(Tasa = (10000 * Cantidad) / Poblacion)

Mapa_Salta <- Mapa_Salta %>%
  left_join(
    Data,
    by = c("etiqueta" = "departamento_nombre")
  )
  
# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- colorRampPalette(c("white", "#ec6230"))(3)

# Grafico
grafico <- ggplot(Mapa_Salta) +
  geom_sf(color="black", aes(fill=Tasa), linewidth=0.5) +
  geom_sf_text(aes(label=formatC(round(Tasa,1), big.mark=".", decimal.mark=",")),
               color="black", family="font_body", size=3, show.legend=FALSE) +
  theme_void() +
  scale_fill_gradient2(name = str_wrap("Tasa de delitos contra la integridad sexual cada 10.000 habitantes (2025)", 40),
                       low = "white", high="#ec6230", midpoint = 5) +
  guides(color = guide_legend(override.aes=list(shape = 16))) +
  # annotation_scale(location = "br", width_hint = 0.2) +
  # annotation_north_arrow(location = "br", which_north = "true", 
  #                        pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
  #                        style = north_arrow_fancy_orienteering) + 
  theme(legend.title = element_text(family="font_title", size=12),
        legend.position = "bottom",
        text = element_text(family="font_body"),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(family="font_body", size=12),
        legend.key.spacing.y = unit(0, "cm"),
        plot.background = element_rect(fill="white", color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=6)


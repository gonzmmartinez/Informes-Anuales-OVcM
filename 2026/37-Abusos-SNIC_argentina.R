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

# Cargar datos
Raw <- read.csv2(
  file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
              "/Datos/snic-departamentos-anual.csv"))
Poblacion <- read.csv(
  file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
              "/Datos/Poblacion_Provincias_Argentina.csv")) %>%
  filter(Año == 2025)

# Modificar datos
Data <- Raw %>%
  filter(codigo_delito_snic_id %in% c("10", "11", "11_1", "11_2", "11_3", "11_4", "11_5"),
         anio == 2025) %>%
  group_by(provincia_nombre) %>%
  summarise(Cantidad = sum(cantidad_hechos), .groups = "drop") %>%
  mutate(provincia_nombre = recode(provincia_nombre, "Ciudad Autónoma de Buenos Aires" = "CABA"),
         provincia_nombre = recode(provincia_nombre, "Tierra del Fuego, Antártida e Islas del Atlántico Sur" = "Tierra del Fuego")) %>%
  left_join(
    Poblacion,
    by = c("provincia_nombre" = "Provincia")
  ) %>%
  mutate(Tasa = (100000 * Cantidad) / Poblacion_total)

# Cargar shape
Mapa_Argentina <- rnaturalearth::ne_states(country = c("argentina"), returnclass = "sf") %>%
  mutate(name = ifelse(name == "Ciudad de Buenos Aires", "CABA", name))

# Modificar datos
Mapa_Argentina <- Mapa_Argentina %>%
  left_join(Data, by=c("name" = "provincia_nombre")) %>%
  replace(is.na(.), 0)

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Grafico
grafico <- ggplot(Mapa_Argentina) +
  geom_sf(color="black", aes(fill=Tasa.x)) +
  geom_sf_text(aes(label=formatC(round(Tasa.x,1), big.mark=".", decimal.mark=",")),
               color="black", family="font_body", size=2, show.legend=FALSE) +
  theme_void() +
  guides(fill = guide_colorbar(direction = "horizontal", barwidth = unit(4, "cm"),
                               theme = theme(legend.frame = element_rect(colour = "black")))) +
  scale_fill_gradient2(name=str_wrap("Tasa de delitos contra la integridad sexual cada 100.000 habitantes (2025)", 30),
                       low="white", high="#c72a29", midpoint=50,
                       breaks=seq(20, 160, by=20)) +
  theme(text=element_text(family="font_body"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.margin = margin(t=0,r=0,b=0,l=0),
        plot.background = element_rect(fill="white", color=NA),
        legend.position = "right",
        legend.justification = "center",
        legend.title = element_text(size=10, family="font_title", hjust=0.5,
                                    margin=margin(t=0,r=0,b=10,l=0)),
        legend.title.position = "top",
        legend.box.just = "center",
        legend.box.margin = margin(l=-40, r=0),
        legend.text = element_text(size=5, family="font_body"),
        legend.ticks = element_line(color="black")) +
  guides(color = guide_legend( 
    override.aes=list(shape = 16)))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=8)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=6, height=8)


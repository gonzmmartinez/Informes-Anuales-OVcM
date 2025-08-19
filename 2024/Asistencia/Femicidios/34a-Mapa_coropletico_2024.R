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
library(ggpp)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Mapa_shape/Salta_deptos_shape/Salta_deptos.shp"))

# Cargar datos
Data_femicidios <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Data_femicidios.csv"))

# Modificar datos
Data_femicidios <- Data_femicidios %>%
  filter(Año == 2024) %>%
  group_by(Localidad) %>%
  summarise(Cantidad = n()) %>%
  ungroup %>%
  rename(etiqueta = "Localidad")

Mapa_Salta <- Mapa_Salta %>%
  left_join(Data_femicidios, by="etiqueta") %>%
  replace(is.na(.), 0) %>%
  mutate(Cantidad = as.factor(Cantidad)) %>%
  mutate(Labels = ifelse(Cantidad != 0, etiqueta, NA))

# Colores
Colores <- colorRampPalette(c("white", "#6e3169"))(4)

# Tabla
Tabla <- Data_femicidios %>%
  rename(Departamento = "etiqueta") %>%
  arrange(desc(Cantidad))

# Theme
my_ttheme <- 
  gridExtra::ttheme_default(base_colour="black", base_size=10, base_family="font",
                            colhead = list(fg_params=list(col="white"),
                                           bg_params=list(fill="#6e3169")),
                            core = list(bg_params=list(fill = "#f2f2f2", col = "white")))

# Grafico
grafico <- ggplot(Mapa_Salta) +
  geom_sf(color="black", aes(fill=Cantidad), linewidth=0.5) +
  annotate(geom="table", x=-68.5, y=-22.25, label = list(Tabla), table.theme=my_ttheme) +
  theme_void() +
  scale_fill_manual(values=Colores) +
  guides(color = guide_legend(override.aes=list(shape = 16))) +
  geom_sf_label(aes(label=Labels, fill=Cantidad), color="white", alpha=0.5, family="font", show_guide=FALSE) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) + 
  theme(legend.title = element_blank(),
        text = element_text(family="font"),
        legend.key.size = unit(0.7, "cm"),
        legend.text = element_text(family="font", size=12),
        legend.key.spacing.y = unit(0, "cm"),
        plot.background = element_rect(fill="white", color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=9, height=6)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=9, height=6)
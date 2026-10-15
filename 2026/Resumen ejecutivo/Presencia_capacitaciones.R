# Limpiar todo
rm(list = ls())

# Librerias
library(openxlsx)
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(cowplot)
library(showtext)
library(stringr)
library(ggimage)
library(ggrepel)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(str_sub(getwd(), 1, -19), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(str_sub(getwd(), 1, -19), "/Mapa_shape/Salta_deptos_shape/Salta_deptos.shp")) %>%
  mutate(etiqueta = ifelse(etiqueta == "San Martín", "General José de San Martín", etiqueta))
  

# Cargar datos
Data <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Mapa_capacitaciones.csv")) %>%
  mutate(ID = as.character(ID)) %>%
  rename(in1 = "ID") %>%
  mutate(Capacitacion = as.logical(Capacitacion)) %>%
  group_by(Departamento) %>%
  summarise(
    Capacitacion = any(Capacitacion, na.rm = TRUE),
    .groups = "drop"
  )

# Modificar datos
Mapa_Salta <- Mapa_Salta %>%
  left_join(Data, by=c("etiqueta" = "Departamento")) %>%
  replace(is.na(.), "Sin dato")

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("TRUE" = "#7c428a",
             "FALSE" = "#cbc2ce")

# Preparar imagenes
Imagen <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Elementos/Pin2.png")

# Definir puntos manualmente
Puntos <- data.frame(
  Etiqueta = c(
    "Pichanal",
    "General Güemes",
    "Rivadavia Banda Sur",
    "Cachi",
    "Tartagal",
    "Embarcación",
    "Dragones",
    "Santa Victoria Este",
    "Yacuy",
    "Carboncito",
    "Aguaray",
    "Orán",
    "Metán",
    "San Antonio de los Cobres",
    "Capital"
  ),
  lat = c(
    -23.322308,
    -24.667905,
    -24.185107,
    -25.120270,
    -22.514047,
    -23.203812,
    -23.249411,
    -22.275875,
    -22.368274,
    -23.235520,
    -22.241952,
    -23.134978,
    -25.495362,
    -24.218620,
    -24.790296
    ),
  lon = c(
    -64.223743,
    -65.049728,
    -62.884700,
    -66.168534,
    -63.799142,
    -64.091835,
    -63.339581,
    -62.713499,
    -63.766874,
    -63.769197,
    -63.736128,
    -64.318623,
    -64.968005,
    -66.318998,
    -65.410377
  )
)

# Transformar coordenadas
Coordenadas <- as.matrix(Puntos[, c("lon", "lat")])

# Preparar dataframe para geom_image()
Imagenes <- data.frame(
  Etiqueta = Puntos$Etiqueta,
  x = Coordenadas[, 1],
  y = Coordenadas[, 2],
  imagen = Imagen
)

# Grafico
Mapa_cor <- ggplot(Mapa_Salta) +
  geom_sf(color="black", aes(fill=Capacitacion)) +
  geom_image(data = Imagenes,
             aes(x = x, y = y + 0.15, image = imagen),
             size = 0.075, inherit.aes = FALSE) +
  geom_label(data=Imagenes, inherit.aes=FALSE,
             aes(x=x, y=y,
                 label=str_wrap(Etiqueta, width=20)),
             family="font_title", size=2, color="black", lineheight = 0.9,
             fill="white", alpha=0.4, nudge_y=-0.05, vjust=1) +
  theme_void() +
  scale_fill_manual(values=Colores) +
  theme(legend.title=element_blank(),
        legend.position="none",
        text=element_text(family="font_title"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.background = element_rect(fill="white", color=NA)) +
  guides(color = guide_legend(override.aes=list(shape = 16)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

# Guardar gr?fico
ggsave(filename=paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=Mapa_cor, dpi=72, width=7, height=6)
ggsave(filename=paste0(filename, ".png"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=Mapa_cor, dpi=100, width=7, height=6)


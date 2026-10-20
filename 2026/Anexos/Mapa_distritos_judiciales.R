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
dir <- paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Mapa_shape/Salta_munic_shape/Salta_shape.shp"))

# Cargar datos
Data <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1TrCkxElnwBcBbpgZ8NZTkoZyKinIzVLMG_y2hPLEd5E/edit?usp=sharing",
                   sheet = "Municipios_distritos") %>%
  mutate(in1 = as.character(in1))

# Modificar datos
Mapa_Salta <- Mapa_Salta %>%
  left_join(Data, by="in1") %>%
  replace(is.na(.), "Sin dato")

# Agrupar por departamentos
Mapa_depart <- Mapa_Salta %>%
  group_by(depart.x) %>%
  summarise()

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Centro" = "#6963aa",
             "Centro - Cafayate" = "#7c428a",
             "Norte - Orán" = "#119ca0",
             "Norte - Tartagal" = "#095e80",
             "Sur - Anta" = "#ec6230",
             "Sur - Metán" = "#c72a29")

# Gráfico
Mapa_cor <- ggplot(Mapa_Salta) +
  geom_sf(color = "white", linewidth=0.1, aes(fill = distrito_judicial)) + 
  geom_sf(data = Mapa_depart, fill = NA, color = "black",
          linewidth = 0.5, inherit.aes = FALSE) +
  annotation_scale(location = "tl", width_hint = 0.2, pad_y = unit(0.8, "in")) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_void() +
  scale_fill_manual(values=Colores,
                    name="Distrito judicial") +
  theme(text=element_text(family="font_title"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(), 
        plot.background = element_rect(fill="white", color=NA),
        legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_text(size=12, family="font_title", face="bold",
                                    margin=margin(r=20)),
        legend.text = element_text(size=12, family="font_title"),
        legend.key.spacing.x = unit(0.5, "cm"),
        legend.margin = margin(b=10)) +
  guides(fill = guide_legend(nrow=2, byrow=FALSE))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

# Guardar gr?fico
ggsave(filename=paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/"),
       plot=Mapa_cor, dpi=72, width=7, height=6)
ggsave(filename=paste0(filename, ".png"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/"),
       plot=Mapa_cor, dpi=100, width=7, height=6)


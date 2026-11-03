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

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(str_sub(getwd(), 1, -30), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Salta_munic_shape/Salta_shape.shp"))

# Cargar datos
Data <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Mapa_poseen_area.csv")) %>%
  mutate(ID = as.character(ID)) %>%
  rename(in1 = "ID")

# Modificar datos
Mapa_Salta <- Mapa_Salta %>%
  left_join(Data, by="in1") %>%
  replace(is.na(.), "Sin dato")

Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce",
            "#d6a62e", "#3f6f9f", "#b05c7a")

# Colores
Colores <- c("Con área" = "#1e7b34",
             "Absorbida" = "#119ca0",
             "Sin área" = "#ec6230",
             "No responde" = "#cbc2ce")

# Grafico
Mapa_cor_2024 <- ggplot(Mapa_Salta) +
  geom_sf(color="black", aes(fill=Area_2024)) +
  labs(title="2024") +
  theme_void() +
  scale_fill_manual(values=Colores) +
  theme(legend.title=element_blank(),
        legend.position = "none",
        text=element_text(family="font_title"),
        plot.title = element_text(family="font_title", size=30, face="bold",
                                  hjust=0.5, margin = margin(t=-20, b=20)),
        plot.subtitle = element_blank(), 
        legend.text = element_text(size=12, family="font_title")) +
  guides(color = guide_legend(override.aes=list(shape = 16)))

Mapa_cor_2026 <- ggplot(Mapa_Salta) +
  geom_sf(color="black", aes(fill=Area_2026)) +
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title="2026") +
  theme_void() +
  scale_fill_manual(values=Colores) +
  theme(legend.title=element_blank(),
        text=element_text(family="font_title"),
        plot.title = element_text(family="font_title", size=40, face="bold", hjust=0.5),
        plot.subtitle = element_blank(), 
        legend.text = element_text(size=15, family="font_title"),
        legend.key.spacing.y = unit(0.5, "cm")) +
  guides(color = guide_legend(override.aes=list(shape = 16)))

Mapa_cor <- plot_grid(Mapa_cor_2024, Mapa_cor_2026, nrow = 1, rel_widths = c(1, 2)) +
  theme(plot.background = element_rect(fill="white", color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

# Guardar gr?fico
ggsave(filename=paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF"),
       plot=Mapa_cor, dpi=70, width=14, height=7)
ggsave(filename=paste0(filename, ".png"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=Mapa_cor, dpi=100, width=14, height=7)
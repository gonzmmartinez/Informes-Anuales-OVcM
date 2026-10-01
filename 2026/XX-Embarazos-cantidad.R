# Limpiar todo
rm(list = ls())

# Funciones
`%nin%` <- function(x, table) !(x %in% table)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)
library(ggforce)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

Data <- data.frame(
  Año = 2024:2025,
  Cantidad = c(123, 187)
)

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data) +
  geom_circle(aes(x0 = (Año - 2023) * 1.75, y0 = 0, r = sqrt(Cantidad / pi) / 10,
                  fill=Cantidad), color = NA) +
  geom_text(aes(x = (Año - 2023) * 1.75, y = 0, label = Cantidad),
    family = "font_body", color = "white", size = 12) +
  geom_text(aes( x = (Año - 2023) * 1.75, y = -1, label = Año),
    family = "font_title", fontface = "bold", color = "black", size = 10) +
  scale_fill_gradient2(high="#4c2158", low="#6963aa") +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        legend.position = "none")

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=4)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=6, height=4)
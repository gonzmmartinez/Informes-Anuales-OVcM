# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Definir colores
Colores <- c("#e54c7c", "#1daa6a")

# Modificar datos
Data <- data.frame(Categoria = c("A", "B"),
                   x=0, y=0,
                   Tamaño=c(100, 0))

# Grafico
grafico <- ggplot(Data, aes(y=y, x=x)) +
  geom_point(aes(color=Categoria, size=Tamaño)) +
  theme_void() +
  scale_x_continuous(limits=c(-1, 1)) +
  scale_y_continuous(limits=c(-1, 1)) +
  scale_color_manual(values=Colores) +
  scale_size_continuous(range=c(62*3, 100*3)) +
  theme(text=element_text(family="font_sans", size=20),
        legend.position="none",
        plot.margin = margin(t=0,r=0,b=0,l=0),
        plot.background = element_rect(fill="white", color=NA),
        plot.caption = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(0, "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=10)
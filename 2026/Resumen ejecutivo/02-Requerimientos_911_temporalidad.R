# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- function(x, table) !(x %in% table)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)
library(forcats)

# Fuentes
library(sysfonts)
library(showtext)

dir <- paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Fonts/")
font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))
showtext_auto()

Data <- data.frame(Tiempo = c("Violencia de género - en curso",
                              "Violencia familiar - en curso",
                              "Violencia de género - histórica",
                              "Violencia familiar - histórica"),
                   Cantidad = c(31979, 41421, 22, 137)) %>%
  mutate(Label = formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg"),
         Tiempo = factor(Tiempo),
         Tiempo = fct_reorder(Tiempo, Cantidad))

# Paleta
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Colores
Colores <- c("Violencia familiar - en curso" = "#119ca0",
             "Violencia de género - en curso" = "#6963aa",
             "Violencia familiar - histórica" = "#a3d5d7",
             "Violencia de género - histórica" = "#B7B3E3")

# Gráfico
grafico <- ggplot(Data, aes(x=Cantidad, y=Tiempo, group=1)) +
  geom_col(aes(fill=Tiempo), width=0.85) +
  geom_text(aes(label=Label), family="font_body", size=5, nudge_x=750,
            fontface="bold", color="black", hjust=0, vjust=0.5) +
  scale_fill_manual(values=Colores) + 
  scale_x_continuous(limits = c(0, 46000), expand=c(0.025,0),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark = ",", format="fg")) +
  theme_light() +
  theme(
    text=element_text(family="font_body"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=12, family="font_body"),
    plot.title=element_blank(),
    plot.subtitle=element_blank(),
    plot.caption=element_blank(),
    axis.text.y=element_text(family="font_subtitle", size=15, margin=margin(r=10)),
    axis.title.y=element_blank(),
    axis.line.y = element_line(color="darkgrey", linewidth=0.5),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin=unit(c(0.25,0.25,0.25,0.25), "cm"),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    panel.grid.major=element_line(linewidth=0.5, color="grey95"),
    panel.grid.major.x=element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=3)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=10, height=3)
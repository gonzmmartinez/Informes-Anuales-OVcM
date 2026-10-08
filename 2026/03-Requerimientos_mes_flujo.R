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

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

Mes_lvl <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
             "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

Data <- Raw %>%
  filter(Año == 2025, Tipo != "Abuso sexual") %>%
  mutate(Mes = factor(Mes, levels = Mes_lvl)) %>%
  group_by(Mes) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup %>%
  mutate(Porcentaje_text = paste0(formatC(round(Porcentaje,1), decimal.mark = ",", format="fg",), "%"),
         hjust = ifelse(Mes %in% c("Enero", "Febrero", "Marzo",
                                   "Abril", "Mayo", "Junio"), 0, 1))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x = Mes, y = Cantidad)) +
  geom_col(aes(fill = Cantidad),
    width = 0.9) +
  geom_text(aes(x=Mes, y=Cantidad*2/3, label=Porcentaje_text),
            hjust = 0.5, vjust = 0.5, family="font_body", color="white", size=4, fontface="bold") +
  geom_text(aes(x=Mes, y=max(Data$Cantidad)*1.1, label=Mes, hjust=hjust),
            vjust=0.5, family="font_title", color="black", size=4) +
  annotate(
    geom = "richtext", x = 0.5, y = -4000,
    label = paste0(
      "<span style='font-size:12pt'>Total:</span><br><b>",
      formatC(sum(Data$Cantidad), big.mark = ".", format = "fg"),
      "</b>"),
    family = "font_title", size = 5, color = "black", fill = NA, label.color = NA, vjust = 1) +
  coord_polar(start = 0) +
  ylim(-2000, NA) +
  scale_y_continuous(limits = c(-4000, NA),
                     expand = c(0.15, 0)) +
  scale_fill_gradient(
    low="#b8d6ac", high="#ec6230"
  ) +
  theme_light() +
  theme(
    text = element_text(family = "font_body"),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(t=-50,b=-50,0,0))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(
  filename = paste0(filename, ".png"),
  path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Graficos/PNG/"),
  plot = grafico, dpi = 100, width = 6, height = 5, bg = "white")

ggsave(filename = paste0(filename, ".pdf"),
  path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Graficos/PDF/"),
  plot = grafico, dpi = 72, width = 6, height = 5, bg = "white")


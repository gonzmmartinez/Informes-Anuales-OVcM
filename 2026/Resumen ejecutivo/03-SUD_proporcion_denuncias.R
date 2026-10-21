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
library(waffle)
library(tidyr)

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

# Crear datos
Data <- data.frame(Año = c(2025, 2026),
                   VFG = c(28.6, 27.8)) %>%
  mutate(Total = 100 - VFG) %>%
  pivot_longer(cols = c(VFG, Total),
               names_to = "Tipo",
               values_to = "Porcentaje") %>%
  mutate(Tipo = factor(Tipo, levels = c("VFG", "Total")))

Cuadros1 <- subset(Data, Año == 2025) %>%
  mutate(Cantidad = round(Porcentaje)) %>%
  slice(rep(row_number(), Cantidad)) %>%
  mutate(Cuadro = row_number(),
         x = (Cuadro - 1) %% 10 + 1,
         y = 10 - (Cuadro - 1) %/% 10)

Cuadros2 <- subset(Data, Año == 2026) %>%
  mutate(Cantidad = round(Porcentaje)) %>%
  slice(rep(row_number(), Cantidad)) %>%
  mutate(Cuadro = row_number(),
         x = (Cuadro - 1) %% 10 + 1,
         y = 10 - (Cuadro - 1) %/% 10)

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores1 <- c("VFG" = "#6963aa",
             "Total" = "#cbc2ce")

Colores2 <- c("VFG" = "#4c2158",
              "Total" = "#cbc2ce")

# Gráfico
grafico1 <- ggplot(Cuadros1, aes(x = x, y = y, fill = Tipo)) +
  geom_tile(width = 0.9, height = 0.9) +
  annotate(geom = "text", x = 5.5, y = -0.5, label = "28,6%",
           family = "font_title", fontface="bold", color="#6963aa", size = 20) +
  annotate(geom = "text", x = 5.5, y = -1.7, label = "42.059 de 147.268 denuncias",
           family = "font_body", size = 6) +
  annotate(geom = "text", x = 5.5, y = 11, label = "2025 - año completo",
           family = "font_title", size = 7, fontface="bold", color="black") +
  scale_fill_manual(values = Colores1) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits = c(-2, 11.5), expand = c(0, 0), breaks = NULL) +
  coord_fixed() +
  theme_void() +
  theme(text=element_text(family="font_body"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
        plot.background = element_rect(fill="white", color=NA))

grafico2 <- ggplot(Cuadros2, aes(x = x, y = y, fill = Tipo)) +
  geom_tile(width = 0.9, height = 0.9) +
  annotate(geom = "text", x = 5.5, y = -0.5, label = "27,8%",
           family = "font_title", fontface="bold", color="#4c2158", size = 20) +
  annotate(geom = "text", x = 5.5, y = -1.7, label = "21.219 de 76.369 denuncias",
           family = "font_body", size = 6) +
  annotate(geom = "text", x = 5.5, y = 11, label = "2026 - 1er semestre",
           family = "font_title", size = 7, fontface="bold", color="black") +
  scale_fill_manual(values = Colores2) +
  scale_x_continuous(expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits = c(-2, 11.5), expand = c(0, 0), breaks = NULL) +
  coord_fixed() +
  theme_void() +
  theme(text=element_text(family="font_body"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
        plot.background = element_rect(fill="white", color=NA))

# Layout
grafico <- plot_grid(grafico1, grafico2, ncol=2,
                     rel_widths = c(1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10.5, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10.5, height=6)

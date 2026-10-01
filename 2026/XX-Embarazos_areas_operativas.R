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
library(tidyr)

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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1cl0-rAT-ARDgQQQIjCApJ_hXtrWXjImAGrFnmzMDukw/edit?usp=sharing",
                  sheet = "Área_operativa",
                  range = "A2:C43")

Data <- Raw %>%
  na.omit() %>%
  mutate(AO = fct_reorder(factor(AO), A_2025, .desc = TRUE)) %>%
  pivot_longer(
    cols = c(A_2024, A_2025),
    names_to = "Año",
    values_to = "Cantidad"
  ) %>%
  mutate(Año = str_sub(Año, start = 3))

Data_segments <- Raw %>%
  na.omit() %>%
  mutate(AO = factor(AO, levels = levels(Data$AO)),
         Diferencia = A_2025 - A_2024,
         Color = ifelse(Diferencia <= 0, "#119ca0", "#c72a29"),
         Numero = sprintf("%+d", Diferencia),
         pos_text = ifelse(Diferencia >0, A_2025 + 1, A_2025 - 1))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("2024" = "grey50",
             "2025" = "black")

# Gráfico
grafico <- ggplot(Data, aes(x = Cantidad, y = AO)) +
  geom_segment(data = Data_segments,
               inherit.aes = FALSE,
               aes(y = AO, x = A_2024, xend = A_2025),
               color = Data_segments$Color,
               linewidth = 3) +
  geom_point(aes(color = Año), size = 3) +
  geom_text(data = Data_segments,
            inherit.aes = FALSE,
            aes(y=AO, x=pos_text, label=Numero),
            color = Data_segments$Color,
            size=4, family="font_body") +
  scale_y_discrete(limits = rev) +
  scale_color_manual(values = Colores) +
  labs(y="Área operativa", x="Cantidad") +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=15, family="font_title", face="bold", margin=margin(r=15)),
        legend.text = element_text(size=15, family="font_title"),
        legend.key.spacing.x = unit(0.5, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color="grey85", size=0.5),
        axis.text.x = element_text(family="font_title", size=12.5, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_title", size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=20),
        axis.title.y = element_text(family="font_title", size=20))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=10)


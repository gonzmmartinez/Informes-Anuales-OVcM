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
font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))
font_add("font_body_b",     file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Hora") %>%
  filter(Accion == "Llamadas")

Dia_lvl <- c("Domingo", "Sábado", "Viernes", "Jueves", "Miércoles", "Martes", "Lunes")
Mes_lvl <- c("Junio", "Mayo", "Abril", "Marzo", "Febrero", "Enero")

Data <- Raw %>%
  filter(Año == 2026) %>%
  mutate(Hora = factor(Hora)) %>%
  group_by(Año, Hora) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

Total <- Data %>%
  filter(Hora %in% c(0,1,2,19,20,21,22,23))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico1 <- ggplot(Data, aes(x = Hora, y = Cantidad)) +
  geom_col(aes(fill = Cantidad),
           width = 0.9) +
  geom_text(aes(x=Hora, y=Cantidad*4/5, label=formatC(Cantidad, big.mark=".", decimal.mark = ",", format="fg")),
            hjust = 0.5, vjust = 0.5, family="font_body", color="white", size=2) +
  coord_polar(start = 0) +
  ylim(-2000, NA) +
  scale_y_continuous(limits = c(-1000, NA),
                     expand = c(0, 0)) +
  scale_fill_gradient(low="#b8d6ac", high="#4c2158") +
  theme_light() +
  theme(
    text = element_text(family = "font_body"),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.border = element_blank(),
    axis.text.x = element_text(size = 12, family = "font_subtitle"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank())

grafico2 <- ggplot() +
  geom_text(aes(x = 1.5, y = 5.05), label = paste0(formatC(round(sum(Total$Porcentaje), 0), big.mark = ".", decimal.mark = ","), "%"), family = "font_title", size = 20, color = "#0f216d", fontface = "bold") +
  geom_text(aes(x = 1.5, y = 4.60), label = paste0("(", formatC(sum(Total$Cantidad), big.mark = ".", decimal.mark = ",", format = "fg"), ")"), family = "font_subtitle", size = 4, color = "black") +
  geom_text(aes(x = 1.5, y = 4.35), label = "de las llamadas por", family = "font_subtitle", size = 5, color = "black") +
  geom_text(aes(x = 1.5, y = 4.0), label = "violencia de género\ny violencia familiar", family = "font_title", size = 5, color = "#0f216d", fontface = "bold", lineheight = 1) +
  geom_text(aes(x = 1.5, y = 3.65), label = "se registraron entre las", family = "font_subtitle", size = 5, color = "black") +
  geom_text(aes(x = 1.5, y = 3.40), label = "19:00 y 3:00 hs", family = "font_title", size = 5, color = "#0f216d", fontface = "bold") +
  coord_cartesian(xlim = c(1, 2), ylim = c(2, 6)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = "white"), panel.background = element_rect(fill = "white", colour = "white"), panel.border = element_blank())

grafico <- plot_grid(grafico1,grafico2,
                     ncol = 2, rel_widths = c(4, 2),
                     align = "h")

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(
  filename = paste0(filename, ".png"),
  path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Graficos/PNG/"),
  plot = grafico, dpi = 100, width = 6.5, height = 4.5, bg = "white")

ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Graficos/PDF/"),
       plot = grafico, dpi = 72, width = 6.5, height = 4.5, bg = "white")


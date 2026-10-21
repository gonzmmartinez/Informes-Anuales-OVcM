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
dir <- paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Datos
Data <- data.frame(Horario = c("Matutino\n(08:01 a 16:00 hs.)", "Vespertino\n(16:01 a 24:00 hs.)", "Nocturno\n(00:01 a 08:00 hs.)"),
                    Cantidad = c(27, 31, 26)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = paste0("<span style='font-size:12.5pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>"))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x = Horario, y = Cantidad)) +
  geom_col(aes(fill = Cantidad),
           width = 0.9) +
  geom_richtext(aes(x = Horario, y=Cantidad/2, label=Label), size=5,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  geom_text(aes(label = Horario, y=40),
            family="font_title", color="black", size=4, hjust=c(0, 1, 0.5)) +
  coord_polar(start = 0) +
  scale_y_continuous(limits = c(-10, 70),
                     expand = c(0, 0)) +
  scale_fill_gradient(low = "#7FC0C2", high = "#119ca0") +
  theme_light() +
  theme(
    text = element_text(family = "font_body"),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title =element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(b=-75, t=-110, -50, -50))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(
  filename = paste0(filename, ".png"),
  path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Graficos/PNG/"),
  plot = grafico, dpi = 100, width = 5, height = 3, bg = "white")

ggsave(filename = paste0(filename, ".pdf"),
  path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Graficos/PDF/"),
  plot = grafico, dpi = 72, width = 5, height = 3, bg = "white")
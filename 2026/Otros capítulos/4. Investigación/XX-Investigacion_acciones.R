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

dir <- paste0(str_sub(dirname(rstudioapi::getActiveDocumentContext()$path), 1, 36),
              "/Fonts/")
font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))
showtext_auto()

# Leer datos
Raw <- read.csv(file = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Investigacion_acciones.csv"))

Data <- Raw %>%
  arrange(desc(Cantidad)) %>%
  mutate(Categoria = fct_reorder(Categoria, Cantidad, .desc = FALSE))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Cantidad, y=Categoria)) +
  geom_segment(aes(y=Categoria, yend=Categoria, x=0, xend=Cantidad, color=Cantidad),
               linewidth = 3) +
  geom_point(aes(y=Categoria, x=Cantidad, color=Cantidad),
             size=7) +
  scale_color_gradient(low="#81D0D4", high="#119ca0") +
  theme_void() +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark = ","), color = Cantidad),
            size=7.5, family="font_body", hjust = 0, nudge_x = 0.5) +
  scale_x_continuous(limits = c(0, 14), expand=c(0,0)) +
  scale_y_discrete(labels = function(z) str_wrap(z, 55)) +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family="font_subtitle", size=15,
                                    margin=margin(r=10, l=20), hjust=1, vjust=0.5),
        plot.background = element_rect(fill="white", color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=8)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=8)

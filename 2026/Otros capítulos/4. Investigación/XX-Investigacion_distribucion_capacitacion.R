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
Raw <- read.csv(file = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Investigacion_distribucion.csv"))

Data <- Raw %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Realizó = factor(Realizó,
                       levels = c("No", "Si_1", "Si_2", "Si_3"))) %>%
  mutate(Label = paste0("<span style='font-size:12pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(Label = ifelse(Porcentaje >= 5, Label, "")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("No" = "#c72a29",
             "Si_1" = "#ec6230",
             "Si_2" = "#6963aa",
             "Si_3" = "#7c428a")

Titulos_leyenda <- c("No" = str_wrap("No, no la realicé", 30),
                     "Si_1" = str_wrap("Sí, realicé cursos de forma voluntaria", 30),
                     "Si_2" = str_wrap("Sí, realicé cursos obligatorios en el trabajo", 30),
                     "Si_3" = str_wrap("Sí, relicé cursos obligatorios y voluntarios", 30))

# Gráfico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Realizó)) +
  geom_rect() +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=4,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores,
                    labels=Titulos_leyenda) +
  theme(text=element_text(family="font_body"),
        legend.position = "right",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_body"),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=3.5)

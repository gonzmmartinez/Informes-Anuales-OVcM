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
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Crear datos
Data <- data.frame(Rango_etario = c("0 - 5 años", "6 - 10 años", "11 - 17 años",
                                    "18 - 21 años", "Más de 21 años"),
                   Cantidad = c(2, 5, 41, 28, 626)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad),
         Rango_etario = factor(Rango_etario,
                               levels = c("0 - 5 años", "6 - 10 años", "11 - 17 años",
                                          "18 - 21 años", "Más de 21 años")))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Rango_etario, y=Cantidad)) +
  geom_col(width=0.85, fill="#1e7b34") +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), format="fg", big.mark=".", decimal.mark=","), "%")),
            family="font_body", color="black", size = 7.5, vjust=0, nudge_y=50,
            fontface="bold") +
  geom_text(aes(label = Cantidad), family="font_body", color="black", size = 5,
            vjust=1, nudge_y=35) +
  labs(y="Cantidad", x="Rango etario") +
  scale_y_continuous(expand = c(0.05, 0), limits=c(0, 700)) +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=12, family="font_title", face="bold", margin=margin(r=15)),
        legend.text = element_text(size=12, family="font_subtitle"),
        legend.key.spacing.x = unit(0.5, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_subtitle", size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_subtitle", size=10, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(family="font_subtitle", size=15),
        axis.title.y = element_text(family="font_subtitle", size=15))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=5)

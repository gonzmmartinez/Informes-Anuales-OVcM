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
Data <- data.frame(Distrito = c("Centro", "Sur-Metán", "Norte-Orán", "Norte-Tartagal"),
                   Tipo = rep(c("Asesoramiento", "Apertura de legajo"), each=4),
                   Cantidad = c(600, 112, 1537, 93, 449, 62, 234, 415)) %>%
  mutate(Tipo = factor(Tipo, levels = c("Apertura de legajo", "Asesoramiento")),
         Distrito = factor(Distrito, levels = c("Centro", "Norte-Orán", "Norte-Tartagal", "Sur-Metán")))

Totales_distrito <- Data %>%
  group_by(Distrito) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Asesoramiento" = "#119ca0",
             "Apertura de legajo" = "#ec6230")

# Gráfico
grafico <- ggplot(Data, aes(x=Distrito, y=Cantidad, fill=Tipo)) +
  geom_col(position="stack", width=0.7) +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark=",", format="fg")),
            family="font_body", position = position_stack(vjust = 0.5), size=5, color="white") +
  geom_text(data=Totales_distrito, inherit.aes=FALSE,
            aes(x=Distrito, y=2000, label=paste0(formatC(round(Porcentaje, 1), big.mark=".", decimal.mark=",", format="fg"), "%")),
            color="black", family="font_body", fontface="bold", size=10, vjust=0, nudge_y=50) +
  geom_text(data=Totales_distrito, inherit.aes=FALSE,
            aes(x=Distrito, y=2000, label=formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg")),
            color="black", family="font_body", size=5, vjust=1, nudge_y=-50) +
  labs(y="Cantidad", x="Distrito judicial") +
  scale_fill_manual(name = str_wrap("Tipo de asistencia", width=20),
                    values = Colores) +
  scale_y_continuous(limits = c(0, 2200)) +
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

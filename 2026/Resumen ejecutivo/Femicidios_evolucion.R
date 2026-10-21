# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
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
Data <- data.frame(Año = 2020:2026,
                   Cantidad = c(11, 11, 11, 12, 9, 6, 7)) %>%
  mutate(Año = as.character(Año),
         Año = ifelse(Año == "2026", "2026*", Año))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, fill=Cantidad)) +
  geom_col(width=0.85) +
  geom_text(aes(label=Cantidad), family="font_body", color="black", nudge_y=max(Data$Cantidad)*0.075,
            size=15) +
  scale_fill_gradient(low="#AC84B4", high="#7c428a") +
  scale_y_continuous(limits=c(0, max(Data$Cantidad)*1.1)) +
  theme_light() +
  theme(text=element_text(family="font_title"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        panel.border = element_blank(),
        axis.text.x = element_text(size=25, margin = margin(t=10,r=0,b=5,l=0)),
        axis.title.x = element_blank(),
        axis.line.x = element_line(linewidth=0.5, color="darkgrey"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=6)

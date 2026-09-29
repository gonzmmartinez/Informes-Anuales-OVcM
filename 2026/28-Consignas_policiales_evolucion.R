# Limpiar todo
rm(list = ls())

# Librer?as
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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rfuD4W7yQsjPiIXeAwo0Hh8nmHlg0NTDpIozghgGsGw/edit?usp=sharing",
                  sheet = "Consignas")

Data <- Raw %>%
  mutate(Año = as.character(Año)) %>%
  group_by(Año, Tipo) %>%
  summarise(Total = sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo, levels=c("Fija", "Ambulatoria", "Personalizada"))) %>%
  mutate(Año = ifelse(Año == "2026", "2026*", Año))

Totales <- Data %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Total))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Fija" = "#119ca0",
             "Ambulatoria" = "#1e7b34",
             "Personalizada" = "#6963aa")

# Grafico 1
grafico <- ggplot(Data, aes(x=Año, y=Total, fill=Tipo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=formatC(Total, big.mark=".", decimal.mark=",", format="fg")),
            position = position_stack(vjust=1), vjust=-0.5, size=3, family="font_body", color="black") +
  geom_point(data=Totales, inherit.aes=FALSE, aes(x=Año, y=40000, size=Cantidad, color=Cantidad), show.legend = FALSE) +
  geom_text(data=Totales, inherit.aes=FALSE, aes(x=Año, y=40000, label=Año),
            size=3, color="white", family="font_title", show.legend = FALSE, nudge_y=1000) +
  geom_text(data=Totales, inherit.aes=FALSE, aes(x=Año, y=40000, label=formatC(Cantidad, big.mark=".", decimal.mark = ",", format="fg")),
            size=4, color="white", family="font_body", fontface="bold", show.legend = FALSE, nudge_y=-1000) +
  theme_light() +
  labs(y="Cantidad",
       caption="* las cantidades corresponden únicamente al primer semestre.") +
  scale_fill_manual(name="Tipo de consigna", values = Colores) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="fg"),
                     limits=c(0, 45000), breaks=seq(0, 30000, by=10000)) +
  scale_color_gradient(high="#7c428a", low="#D1B0D9") +
  scale_size_continuous(range=c(20, 25)) +
  theme(text=element_text(family="font_sans"),
        legend.position = "top",
        legend.justification = "right",
        legend.title = element_text(size=12, family="font_title", face="bold"),
        legend.text = element_text(size=12, family="font_title"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=7.5, family="font_title", margin=margin(t=5)),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        axis.text.x = element_text(family="font_title", size=15, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_title", size=10, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=12),
        axis.title.y = element_text(family="font_title", size=12))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=5)
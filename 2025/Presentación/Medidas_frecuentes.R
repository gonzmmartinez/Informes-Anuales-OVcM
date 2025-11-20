# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)
library(forcats)
library(ggrounded)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Data <- data.frame(Medida = c("Abstención de actos de violencia", "Consignas policiales ambulatorias",
                              "Prohibición de acercamiento", "Consignas policiales fijas"),
                   Porcentaje = c(18.3, 15.7, 13.7, 10.1)) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Medida = fct_reorder(Medida, Porcentaje, .desc = TRUE))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Abstención de actos de violencia" = "#852f8c",
             "Consignas policiales ambulatorias" = "#ff9d27",
             "Prohibición de acercamiento" = "#ff621d",
             "Consignas policiales fijas" = "#206170")

# Total
Total <- sum(Data$Cantidad)

# Gráfico
grafico <- ggplot(Data, aes(x=Medida, y=Porcentaje, fill=Medida)) +
  geom_col_rounded(radius = grid::unit(10, "pt")) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font_sans", color="black", nudge_y=3, size=15, fontface="bold") +
  scale_fill_manual(values=Colores) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(limits=c(0, max(Data$Porcentaje)*1.5), labels = function(z) round(z,0), expand = c(0,0)) +
  theme_void() +
  theme(text=element_text(family="font_sans"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.x = element_text(family="font_sans", size=20, lineheight = 0.8, margin=margin(t=15), vjust=1),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill=NA, color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=5)
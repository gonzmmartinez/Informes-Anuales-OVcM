# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Abusos_MPF.csv"))

Data1 <- Raw %>%
  filter(Año == 2023) %>%
  group_by(Mes, Mes_ord) %>%
  summarise(Cantidad = sum(Cantidad))

Data2 <- Raw %>%
  filter(Año == 2022) %>%
  group_by(Mes, Mes_ord) %>%
  summarise(Cantidad = sum(Cantidad))

# Colores
Colores <- c("#6e3169", "#ec6489","#FE6244")

# Gráfico
grafico1 <- ggplot(Data1, aes(x=reorder(Mes, Mes_ord), y=Cantidad, group="1")) +
  geom_line(linewidth=2, color = Colores[1]) +
  geom_point(size=3, color = Colores[1]) +
  geom_text(aes(label=Cantidad), size=3, family="font", vjust=-1.5, show_guide=FALSE) +
  theme_light() +
  ylim(0,300) +
  labs(title="2023", x="Mes", y="Cantidad") +
  scale_x_discrete(labels = function(z) str_sub(z, 0, 3)) +
  theme(text=element_text(family="font"),
        legend.position = "bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=30, family="font", face="bold", hjust=0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

# Gráfico
grafico2 <- ggplot(Data2, aes(x=reorder(Mes, Mes_ord), y=Cantidad, group="1")) +
  geom_line(linewidth=1.5, color = Colores[2]) +
  geom_point(size=2.5, color = Colores[2]) +
  geom_text(aes(label=Cantidad), size=2, family="font", vjust=-1.5, guides=FALSE) +
  theme_light() +
  ylim(0,300) +
  labs(title="2022") +
  scale_x_discrete(labels = function(z) str_sub(z, 0, 3)) +
  theme(text=element_text(family="font"),
        legend.position = "none",
        plot.title = element_text(size=20, family="font", face="bold", hjust=0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=8, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=8, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t=20, r=100, b=10, l=100))

# Layout
grafico <- plot_grid(grafico2, grafico1, nrow=2, rel_heights = c(2,3))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=8)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=8, height=8)
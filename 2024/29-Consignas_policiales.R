# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Data <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Consignas_policiales.csv")) %>%
  mutate(Año = as.factor(Año))

# Definir colores
Colores <- c("#e54c7c", "#1daa6a")

# Modificar datos
Data <- Data %>%
  group_by(Año, Sujeto) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = round(100 * Cantidad/sum(Cantidad),1)) %>%
  ungroup() %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",Porcentaje,"%**</span><br><span style='font-size:10pt'>",Cantidad,"</span>"))

# Titulo
titulo <- ggplot() +
  labs(title="Consignas policiales a víctimas y agresores (VIF-VG)",
       subtitle="Años 2021-2023. Provincia de Salta") +
  theme_void() +
  theme(plot.title=element_text(family="font", size=20, face="bold"),
        plot.subtitle=element_text(family="font", size=15),
        plot.margin = margin(t=20, r=0, b=0, l=10))

# Grafico 1
grafico1 <- ggplot(Data %>%
                     filter(Año == 2021) %>%
                     mutate(ymax = cumsum(Porcentaje)) %>%
                     mutate(ymin = c(0, head(ymax, n=-1))) %>%
                     rowwise() %>%
                     mutate(ymid = ymax - (ymax - ymin)/2) %>%
                     ungroup(),
                   aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sujeto)) +
  geom_rect() +
  coord_polar(theta="y") +
  theme_void() +
  geom_richtext(aes(x=4, y=ymid, label = Label), color = "black", label.color = NA,
                family="font", show.legend=FALSE, fill=NA, nudge_x=0.8, size=4) +
  annotate(geom="text", x=1, y=0, label="2.021", size=12, family="font", fontface="bold", color="black") +
  xlim(1,5) +
  scale_fill_manual(name="Destinatario de la consigna", values=Colores[c(1,2)]) +
  theme(text=element_text(family="font", size=20),
        legend.position="none",
        plot.margin = margin(t=25,r=0,b=0,l=0),
        legend.title = element_blank())

# Grafico 2
grafico2 <- ggplot(Data %>%
                     filter(Año == 2022) %>%
                     mutate(ymax = cumsum(Porcentaje)) %>%
                     mutate(ymin = c(0, head(ymax, n=-1))) %>%
                     rowwise() %>%
                     mutate(ymid = ymax - (ymax - ymin)/2) %>%
                     ungroup(),
                   aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sujeto)) +
  geom_rect() +
  coord_polar(theta="y") +
  theme_void() +
  geom_richtext(aes(x=4, y=ymid, label = Label), color = "black", label.color = NA,
                family="font", show.legend=FALSE, fill=NA, nudge_x=0.8, size=4) +
  annotate(geom="text", x=1, y=0, label="2.022", size=12, family="font", fontface="bold", color="black") +
  xlim(1,5) +
  scale_fill_manual(name="Destinatario/a de la consigna", values=Colores[c(1,2)]) +
  theme(text=element_text(family="font", size=20),
        legend.position="top",
        legend.justification = "center",
        legend.title = element_text(family="font", size=10, margin=margin(r=-15)),
        plot.margin = margin(t=25,r=0,b=0,l=0))

# Grafico 3
grafico3 <- ggplot(Data %>%
                     filter(Año == 2023) %>%
                     mutate(ymax = cumsum(Porcentaje)) %>%
                     mutate(ymin = c(0, head(ymax, n=-1))) %>%
                     rowwise() %>%
                     mutate(ymid = ymax - (ymax - ymin)/2) %>%
                     ungroup(),
                   aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sujeto)) +
  geom_rect() +
  coord_polar(theta="y") +
  theme_void() +
  geom_richtext(aes(x=4, y=ymid, label = Label), color = "black", label.color = NA,
                family="font", show.legend=FALSE, fill=NA, nudge_x=0.8, size=4) +
  annotate(geom="text", x=1, y=0, label="2.023", size=12, family="font", fontface="bold", color="black") +
  xlim(1,5) +
  scale_fill_manual(name="", values=Colores[c(1,2)]) +
  theme(text=element_text(family="font", size=20),
        legend.position="none",
        plot.margin = margin(t=25,r=0,b=0,l=0))

# Grid
grafico <- plot_grid(grafico1, grafico2, grafico3, ncol=3, align="h") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=4)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=10, height=4)
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
Raw <- read.csv2(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Capacitaciones_tematicas.csv"))

Data <- Raw %>%
  mutate(Porcentaje = 100 * Cantidad/sum(Cantidad)) %>%
  arrange(Porcentaje) %>%
  mutate(Ord = row_number()) %>%
  mutate(Label = paste0("<span style='font-size:30pt'>**",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                        "** </span><span style='font-size:20pt'>(",
                        formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                        "%)</span>"))

# Gráfico
grafico <- ggplot(Data, aes(x=Cantidad, y=reorder(Temática, Ord))) +
  geom_col(aes(fill=Cantidad)) +
  scale_fill_gradient(high="#f06464", low="#f08035") +
  theme_classic() +
  labs(y="Temática de la capacitación", x="Cantidad") +
  geom_richtext(aes(label = Label),
                color = ifelse(Data$Porcentaje <= 5, "black", "white"), label.color = NA,
                show.legend=FALSE, fill=NA,
                family="font", hjust = ifelse(Data$Porcentaje <= 5, -0.2, 1.2)) +
  scale_y_discrete(labels = function(z) str_wrap(z, 30)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.background = element_blank(), legend.box.background = element_rect(color = "black"),
        legend.box.margin=margin(5,5,5,5),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=20, margin = margin(t=0,r=10,b=0,l=0)),
        axis.title.x = element_text(size=20, margin = margin(t=15, r=0, b=0, l=0)),
        axis.title.y = element_text(size=20, margin = margin(t=0, r=15, b=0, l=0)))

# Guardar gráfico
ggsave(filename="Capacitaciones_tematicas.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=15, height=15)
ggsave(filename="Capacitaciones_tematicas.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=15, height=15)
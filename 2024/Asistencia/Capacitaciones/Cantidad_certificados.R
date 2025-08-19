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

Data <- data.frame(Genero = factor(c("Personas certificadas", "No"),
                                   levels = c("No", "Personas certificadas")),
                   Cantidad = c(84, 27)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje <= 30, NA, paste0("<span style='font-size:25pt'>**",
                                                 formatC(round(Porcentaje,1),
                                                         big.mark = ".",
                                                         decimal.mark = ","),
                                                 "%**</span><span style='font-size:15pt'> (",
                                                 formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                                                 ")</span>")))

Paleta <- c("#6e3169", "#a5549c", "#ec6489", "#f2904c", "#f7bc7c", "#72bf90", "#1daa6a")

# Definir colores
Colores <- c("Personas certificadas" = "#f2904c", "#bdbebd")

# Gráfico1
grafico <- ggplot(Data, aes(y="Genero", x=Porcentaje, fill=Genero)) +
  geom_col(aes(fill=Genero), position = "fill") +
  geom_richtext(aes(x = 0.40, label=Label), color="white", label.color = NA, family="font", show.legend=FALSE, fill=NA) +
  geom_text(aes(y = 2, x=0.50, label="")) +
  geom_richtext(aes(y = 1.7, x=0.50, label="**111** personas se inscribieron en total"), size=7, color="black", label.color = NA, family="font", show.legend=FALSE, fill=NA) +
  theme_light() +
  scale_fill_manual(values=Colores, limits=c("Personas certificadas"), labels = function(x) str_wrap(x, width = 30)) + 
  scale_x_continuous(labels = function(z) paste0(round(100 * z), "%")) +
  theme(text=element_text(family="font"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(family="font", size=15, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.text = element_text(size=10, margin=margin(t=10, b=10, r=5, l=5)),
        legend.box.margin=margin(5,5,5,5),
        plot.background = element_rect(fill = "white", colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank())

# Guardar gráfico
ggsave(filename="Cantidad_certificados.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=3)
ggsave(filename="Cantidad_certificados.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=7, height=3)
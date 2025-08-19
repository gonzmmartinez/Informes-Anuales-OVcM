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

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Denuncias_SUD.csv"))

Data <- Raw %>%
  group_by(Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        paste0("<span style='font-size:15pt'>**",round(Porcentaje,1),"%**</span><br><span style='font-size:10pt'>",formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")),
                        paste0("<span style='font-size:30pt'>**",round(Porcentaje,1),"%**</span><br><span style='font-size:10pt'>",formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>"))),
         ypos = (sum(Cantidad) - lag(cumsum(Cantidad), default = 0) - 0.5 * Cantidad))

# Definir colores
Colores <- c("#f06464", "#1d70b7", "#006633", "#e43164","#6e4684","#f08035","#747264")

# Gráfico1
grafico <- ggplot(Data, aes(x="", y=Cantidad, fill=Tipo)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0, direction = -1) +
  theme_void() +
  geom_richtext(aes(y = ypos, label = Label),
                color = ifelse(Data$Porcentaje >= 5, "white", "black"),
                nudge_x = ifelse(Data$Porcentaje >=5, 0.15, 0.7),
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
ggsave(filename="Denuncias_SUD_tipo.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=5)
ggsave(filename="Denuncias_SUD_tipo.pdf", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=7, height=5)
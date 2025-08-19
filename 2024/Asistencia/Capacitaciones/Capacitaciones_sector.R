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

Data <- data.frame(Categoria = factor(c("EMPRESAS. Equipos de OSC, ONG y/o áreas privadas",
                                  "SINDICATOS. Equipos de OSC, ONG y/o áreas privadas",
                                  "Equipos de OSC, ONG y/o áreas privadas",
                                  "Equipos estatales de cualquier poder y nivel del Estado"),
                                  levels= c("EMPRESAS. Equipos de OSC, ONG y/o áreas privadas",
                                            "SINDICATOS. Equipos de OSC, ONG y/o áreas privadas",
                                            "Equipos de OSC, ONG y/o áreas privadas",
                                            "Equipos estatales de cualquier poder y nivel del Estado")),
                   Cantidad = c(9,2,3,13)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(ypos = (sum(Cantidad) - lag(cumsum(Cantidad), default = 0) - 0.5 * Cantidad)) %>%
  mutate(Label = paste0("<span style='font-size:20pt'>**",
                        formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                                "</span>"))

# Definir colores
Colores <- c("EMPRESAS. Equipos de OSC, ONG y/o áreas privadas" = "#9900ff",
             "SINDICATOS. Equipos de OSC, ONG y/o áreas privadas" = "#8e7cc3",
             "Equipos de OSC, ONG y/o áreas privadas" = "#b8add2",
             "Equipos estatales de cualquier poder y nivel del Estado" = "#34a853")

# Gráfico1
grafico <- ggplot(Data, aes(x="", y=Cantidad, fill=Categoria)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0, direction = -1) +
  theme_void() +
  geom_richtext(aes(y = ypos, label = Label), color = "white", label.color = NA, family="font",
                show.legend=FALSE, fill=NA, nudge_x=0.2) +
  scale_fill_manual(values=Colores, labels = function(x) str_wrap(x, width = 30)) + 
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=15, margin=margin(t=10, b=10, r=5, l=5)),
        legend.box.margin=margin(5,5,5,5),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
ggsave(filename="Capacitaciones_sector.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename="Capacitaciones_sector.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=10, height=5)
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

Data <- data.frame(Genero = factor(c("Mujeres cis", "Varones cis", "Sin dato"),
                                   levels=c("Mujeres cis", "Varones cis", "Sin dato")),
                   Cantidad = c(20,36,1)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(ypos = (sum(Cantidad) - lag(cumsum(Cantidad), default = 0) - 0.5 * Cantidad)) %>%
  mutate(Label = paste0("<span style='font-size:25pt'>**",
                        formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                        "%**</span><br><span style='font-size:15pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                                "</span>"))

Paleta <- c("#6e3169", "#a5549c", "#ec6489", "#f2904c", "#f7bc7c", "#72bf90", "#1daa6a")

# Definir colores
Colores <- c("Mujeres cis" = "#f2904c",
             "Varones cis" = "#72bf90",
             "Sin dato" = "#bdbebd")

# Gráfico1
grafico <- ggplot(Data, aes(x="", y=Cantidad, fill=Genero)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta="y", start=0, direction = -1) +
  theme_void() +
  geom_richtext(aes(y = ypos, label = Label), color = ifelse(Data$Porcentaje >= 5, "white", "black"), label.color = NA, family="font",
                show.legend=FALSE, fill=NA, nudge_x = ifelse(Data$Porcentaje >=5, 0.1, 0.65)) +
  scale_fill_manual(name=str_wrap("Género de las personas que certificaron",25), values=Colores, labels = function(x) str_wrap(x, width = 30)) + 
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=15, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_text(family="font", size=15, margin=margin(t=0, r=0, b=10, l=0)),
        legend.text = element_text(size=15, margin=margin(t=10, b=10, r=5, l=5)),
        legend.box.margin=margin(5,5,5,5),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
ggsave(filename="Diputados_genero.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=5)
ggsave(filename="Diputados_genero.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=7, height=5)
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
Data <- data.frame(Categoria = factor(c("Sí", "No"), levels=c("Sí", "No")),
                   Cantidad = c(12, 40)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Orden = row_number()) %>%
  mutate(Label = paste0("<span style='font-size:30pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:20pt'>(",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                        ")</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2)

# Otros colores
Colores_raw <- c("#6e3169", "#e54c7c", "#f2904c", "#ffd241", "#1daa6a", "#747264")

# Definir colores
Colores <- c("Sí" = "#a5549c",
             "No" = "#e6e8eb")

# Total
Total <- paste0(paste0("<span style='font-size:30pt'>",
                       "Total",
                       "</span><br><span style='font-size:70pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gráfico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.75, fill=Categoria)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=17,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.375, y=ymid, label=Label), size = 7,
                color = "black", label.color = NA, family="font", show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  scale_fill_manual(values=Colores, labels = function(z) str_wrap(z, width=40)) +
  xlim(c(1.5, 5)) +
  theme_void() +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=35, family="font"),
        plot.margin = margin(t=-150, b=-150, r=0, l=-150),
        legend.box.margin = margin(t=0, r=0, b=0, l=-150),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
ggsave(filename="Municipios_presupuesto.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=9, height=7)
ggsave(filename="Municipios_presupuesto.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=9, height=7)
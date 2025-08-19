# Limpiar todo
rm(list = ls())
options(scipen=999)

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
Data <- data.frame(Categoria = factor(c("Mujeres", "Varones", "Personal trans y NB"), levels=c("Mujeres", "Varones", "Personal trans y NB")),
                   Cantidad = c(4362, 7095, 48)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Orden = row_number()) %>%
  mutate(Label = ifelse(Porcentaje <= 10,
                        paste0("<span style='font-size:20pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                               "%**</span><br><span style='font-size:15pt'>(",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                               ")</span>"),
                        paste0("<span style='font-size:30pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                               "%**</span><br><span style='font-size:20pt'>(",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                               ")</span>"))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2)

# Otros colores
Colores_raw <- c("#6e3169", "#e54c7c", "#f2904c", "#ffd241", "#1daa6a", "#747264")

# Definir colores
Colores <- c("Mujeres" = "#f2904c",
             "Varones" = "#a5549c",
             "Personal trans y NB" = "#1daa6a")

# Total
Total <- paste0(paste0("<span style='font-size:30pt'>",
                       "Total",
                       "</span><br><span style='font-size:40pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ",", format="d"),
                       "**</span>"))

# Gráfico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.75, fill=Categoria)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=15,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(y=ymid, label=Label), x=ifelse(Data$Porcentaje <= 10, 4.4, 3.375), size = 7,
                color = "black", label.color = NA, family="font", show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  scale_fill_manual(values=Colores, labels = function(z) str_wrap(z, width=10)) +
  xlim(c(1.5, 6)) +
  theme_void() +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_blank(),
        legend.text = element_text(size=25, family="font"),
        plot.margin = margin(t=-150, b=-200, r=-50, l=-250),
        legend.box.margin = margin(t=0, r=0, b=0, l=-200),
        legend.key.spacing.y = unit(1, 'cm'),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
ggsave(filename="Personal_municipal.png", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=9, height=7)
ggsave(filename="Personal_municipal.svg", path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/SVG/"),
       plot=grafico, dpi=72, width=9, height=7)
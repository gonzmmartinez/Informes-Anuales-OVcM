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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Abusos_911.csv"))

Data <- Raw %>%
  filter(Año == 2024, Tipo == "Abuso sexual") %>%
  group_by(Accion) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Accion = factor(Accion,
                         levels = c("Llamadas", "Intervenciones", "Intervenciones SAMEC"))) %>%
  arrange(Accion) %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Definir colores
Colores <- c("Llamadas" = "#f2904c",
             "Intervenciones" = "#1daa6a",
             "Intervenciones SAMEC" = "#ec6489")

# Total
Total <- paste0(paste0("<span style='font-size:20pt'>",
                       "Total",
                       "</span><br><span style='font-size:50pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gráfico1
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Accion)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=12,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label),
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name="Tipo de requerimiento", values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=4.5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=7, height=4.5)
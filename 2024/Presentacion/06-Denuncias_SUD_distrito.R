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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Denuncias_SUD.csv")) %>%
  filter(Año == 2024)

Data <- Raw %>%
  group_by(Distrito) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  arrange(desc(Cantidad)) %>%
  mutate(Orden = row_number())

# Colores
Colores <- c("Centro" = "#f2904c",
             "Cafayate" = "#f2904c",
             "Orán" = "#f7bc7c",
             "Tartagal" = "#a5549c",
             "Anta" = "#72bf90",
             "Metán" = "#1daa6a")

# Grafico
grafico <- ggplot(Data, aes(x=reorder(Distrito, Orden), y=Porcentaje, fill=Distrito)) +
  geom_col() +
  geom_richtext(aes(label = paste0("<span style='font-size:15pt'>**",round(Porcentaje,1),
                                   "%**</span><br><span style='font-size:8pt'>",
                                   formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>"))),
                color = "black",
                vjust = ifelse(Data$Porcentaje <=10, -0.2, 1.2),
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  labs(x="Distrito judicial", y="Porcentaje") +
  scale_fill_manual(values = Colores) +
  scale_y_continuous(labels=function(z) paste0(abs(z), "%")) +
  theme_light() +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=10, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=0,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(size=12, margin = margin(t=5,r=0,b=5,l=0)),
        axis.title.y = element_text(size=12, margin = margin(t=0,r=5,b=0,l=0)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=6)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=6)

# Fuente: Elaboración propia a partir de la información remitida por la Oficina de Violencia Familiar y de Género. Corte de Justicia de Salta
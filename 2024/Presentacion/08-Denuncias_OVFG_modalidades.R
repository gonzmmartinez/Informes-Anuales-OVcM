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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Denuncias_modalidades.csv"))

Levels <- (Raw %>%
  filter(Año == 2024, Modalidad != "Sin especificar") %>%
  group_by(Año, Modalidad) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  arrange(desc(Porcentaje)) %>%
  ungroup %>%
  filter(Modalidad != "Otras") %>%
  add_row(Modalidad = "Otras"))$Modalidad

Data1 <- Raw %>%
  filter(Año == 2024, Modalidad != "Sin especificar") %>%
  mutate(Modalidad = factor(Modalidad, levels=Levels)) %>%
  group_by(Año, Modalidad) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje >= 10,
                        paste0("<span style='font-size:20pt'>**",formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ","),"%**</span><br><span style='font-size:10pt'>",formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")),
                        NA)) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, Levels[1], paste0(Modalidad, "    (", formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ","), "%)")))

# Definir colores
Colores <- c("Doméstica" = "#e54c7c",
             "Acoso callejero" = "#6e3169",
             "Institucional" = "#f2904c",
             "Laboral" = "#ffd241",
             "Mediática" = "#1daa6a",
             "Obstétrica" = "#4cb2f2",
             "Política" = "#f24c7e",
             "Otras" = "#747264")

# Gráfico1
grafico <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Modalidad)) +
  geom_rect() +
  geom_richtext(aes(x = 3.2, y=ymid, label=Label), size=4,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Modalidad de violencia", width = 30),
                    values = Colores,
                    labels = Data1$Leyenda) +
  labs(title="2.024",
       subtitle = "enero-septiembre") +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font", size=15, face="italic", hjust=0.5),
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=4.5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=6, height=4.5)
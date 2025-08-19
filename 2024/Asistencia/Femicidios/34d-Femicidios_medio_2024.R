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
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Data_femicidios.csv"))

Data <- Raw %>%
  filter(Año == 2024) %>%
  select(Medio.utilizado) %>%
  group_by(Medio.utilizado) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")),
                        paste0("<span style='font-size:18pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2)

Data <- Data %>%
  mutate(Medio.utilizado = format(Medio.utilizado, levels = (Data %>% arrange(desc(Porcentaje)))$Medio.utilizado))

# Definir colores
Paleta <- c("#6e3169", "#e54c7c", "#f2904c", "#ffd241", "#1daa6a", "#1da1aa", "#747264")

Colores <- c("Ahorcamiento" = "#f2904c",
             "Arma blanca" = "#e54c7c",
             "Arma de fuego" = "#1da1aa")

# Gráfico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=Medio.utilizado)) +
  geom_rect() +
  geom_richtext(aes(x = ifelse(Porcentaje <= 5, 4.4, 3.3), y=ymid, label=Label),
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name="Medio utilizado", values = Colores, labels = function(z) str_wrap(z, width=15)) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=15),
        legend.box.margin = margin(t=5,b=5,l=-40,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-50, r=-50, b=-50, l=-70),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=7, height=5)
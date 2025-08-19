# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Data_femicidios.csv"))

Data <- Raw %>%
  filter(Año == 2023) %>%
  select(Lugar.del.hecho) %>%
  group_by(Lugar.del.hecho) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

# Gráfico
grafico <- ggplot(Data, aes(x=Lugar.del.hecho, y=Cantidad, fill=Lugar.del.hecho)) +
  geom_col() +
  geom_text(aes(label=Cantidad), family="font", color="black", vjust=-1, size=8) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font", color="white", vjust=2, size=10, fontface="bold") +
  geom_text(aes(label=Lugar.del.hecho), family="font", color="white", vjust=3.5, size=10) +
  labs(x="Rango etario", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_manual(values = c("#f2904c", "#1daa6a"), labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(breaks=1:3, limits=c(0, max(Data$Cantidad)+0.5), labels = function(z) round(z,0)) +
  theme_void() +
  theme(text=element_text(family="font"),
        legend.position="none",
        panel.grid = element_blank(),
        plot.background = element_rect(fill="white", colour=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=7, height=7)

# Fuente: Elaboración propia a partir de la información remitida por la Oficina de Violencia Familiar y de Género. Corte de Justicia de Salta
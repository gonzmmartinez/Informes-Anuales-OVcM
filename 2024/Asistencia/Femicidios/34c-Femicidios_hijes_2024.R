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

Hijes <- c("Sin hijas/os", "1 hija/o", "2 hijas/os", "3 hijas/os", "4 hijas/os", "5 hijas/os", "No informa")

Data <- Raw %>%
  filter(Año == 2024) %>%
  select(Hijos) %>%
  mutate(Hijos = factor(Hijos, levels=Hijes)) %>%
  group_by(Hijos) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

# Gráfico
grafico <- ggplot(Data, aes(x=Hijos, y=Cantidad, fill=Cantidad)) +
  geom_col() +
  geom_text(aes(label=Cantidad), family="font", color="black", vjust=-1, size=8) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font", color="white", vjust=2, size=10, fontface="bold") +
  labs(x="Cantidad de hijas/os", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_gradient2(low="#6e3169", high="#1da1aa", mid="#ec6489", labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(limits=c(0, max(Data$Cantidad)+1), labels = function(z) round(z,0)) +
  theme_light() +
  theme(text=element_text(family="font"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=20, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=12, height=7)

# Fuente: Elaboración propia a partir de la información remitida por la Oficina de Violencia Familiar y de Género. Corte de Justicia de Salta
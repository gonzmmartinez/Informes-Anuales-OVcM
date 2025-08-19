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

Edades <- c("Menos de 18 años", "18-29 años", "30-39 años", "40-49 años", "50 años o más")

Data <- Raw %>%
  filter(Año == 2024) %>%
  select(Rango.etario) %>%
  group_by(Rango.etario) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

Data <- as.data.frame(Edades) %>%
  left_join(Data, by = join_by(Edades == Rango.etario)) %>%
  mutate(Cantidad = replace_na(Cantidad, 0),
         Porcentaje = replace_na(Porcentaje, 0)) %>%
  mutate(Edades = factor(Edades, levels=Edades))

# Gráfico
grafico <- ggplot(Data, aes(x=Edades, y=Cantidad, fill=Cantidad)) +
  geom_col() +
  geom_text(aes(label=Cantidad), family="font", color="black", vjust=-1, size=8) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font", color="white", vjust=2, size=10, fontface="bold") +
  labs(x="Rango etario", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_gradient2(low="#6e3169", high="#1da1aa", mid="#ec6489", labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(breaks=1:3, limits=c(0, max(Data$Cantidad)+1), labels = function(z) round(z,0)) +
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
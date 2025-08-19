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
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Requerimientos_911_años.csv"))

Data <- Raw %>%
  mutate(Año = as.factor(formatC(Año, big.mark = ".", decimal.mark = ",")))

# Colores
Colores <- c("#6e3169", "#ec6489")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, group=1)) +
  geom_col(aes(alpha=Cantidad), fill=Colores[1]) +
  geom_line(linewidth = 4, color=Colores[2]) +
  geom_point(size = 5, color=Colores[2]) +
  geom_text(aes(y=50000, label=formatC(Cantidad, big.mark=".", decimal.mark=",")), size=12, family="font", fontface="bold", color="white") +
  labs(title="",
       x="Año", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_gradient(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d")) +
  scale_y_continuous(limits = c(0, 100000), labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d")) +
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
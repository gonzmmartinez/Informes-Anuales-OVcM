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
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Requerimientos_911_año_VFG.csv"))

Data <- Raw %>%
  mutate(Año = as.factor(formatC(Año, big.mark=".", decimal.mark=",")),
         Tipo = factor(Tipo, levels=c("Violencia familiar", "Violencia de género"))) %>%
  mutate(Porcentaje = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=","), "%"))

# Definir colores
Colores <- c("Violencia de género" = "#f2904c",
             "Violencia familiar" = "#ec6489",
             "Violencia familiar histórica" = "#6e3169")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_hline(yintercept = 0.5, color="gray", linetype=2) +
  geom_col(position="fill", width=0.7) +
  geom_text(aes(label = Porcentaje), family="font",
            position = position_fill(vjust = 0.5), size=8) +
  theme_light() +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_fill_manual(name = str_wrap("Situación motivo del requerimiento", width=20),
                    values = Colores) +
  theme(text=element_text(family="font"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font"),
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
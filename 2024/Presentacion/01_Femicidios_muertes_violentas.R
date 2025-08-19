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
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Femicidios_muertes_violentas.csv"))

Data <- Raw %>%
  mutate(Año = as.factor(formatC(Año, big.mark=".", decimal.mark=",")),
         Femicidio = factor(Femicidio, levels = c("Muertes violentas", "Femicidios caratulados"))) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=","), "%")) %>%
  mutate(Cantidad = ifelse(Cantidad == 0, NA, Cantidad)) %>%
  na.omit()

# Definir colores
Paleta <- c("#6e3169",
            "#e54c7c",
            "#f2904c",
            "#ffd241",
            "#1daa6a",
            "#747264",
            "#c93131",
            "#e55a3e",
            "#9bc6b7",
            "#266f9b",
            "#2e544d")

# Definir colores
Colores <- c("Femicidios caratulados" = "#ec6489",
             "Muertes violentas" = "#9bc6b7",
             "Violencia familiar histórica" = "#6e3169")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, fill=Femicidio)) +
  geom_col(position="dodge", width=0.8) +
  geom_text(aes(y=Cantidad/2, label = Cantidad), family="font", position = position_dodge2(padding = 0.2, width=0.8),
            size=8, fontface="bold") +
  theme_light() +
  scale_y_continuous() +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major.y = element_line(colour = "#F5F5F5"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size=20, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=7)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=7)

# Fuente: Elaboración propia a partir de la información remitida por la Oficina de Violencia Familiar y de Género. Corte de Justicia de Salta
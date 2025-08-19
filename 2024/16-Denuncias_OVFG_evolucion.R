# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Crear datos
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Datos/Denuncias_OVFG.csv"))

Data <- Raw %>%
  mutate(Año = factor(Año), Trimestre = factor(Trimestre)) %>%
  filter(Año %in% c(2023, 2024)) %>%
  filter(Tipo %in% c("Familiar", "Género")) %>%
  group_by(Año, Trimestre) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  ungroup %>%
  mutate(Orden = row_number(),
         Label = paste0(Trimestre, "-", str_sub(Año, 3,4)))

# Colores
Colores <- c("#6e3169", "#ec6489")

# Gráfico
grafico <- ggplot(Data, aes(x=reorder(Label, Orden), y=Cantidad, group="1")) +
  geom_vline(xintercept=4.5, linetype=2, color="grey") +
  geom_col(aes(alpha=Cantidad), fill=Colores[1]) +
  geom_line(linewidth=2, color=Colores[2]) +
  geom_point(size=3, color=Colores[2]) +
  geom_text(aes(y=4000, label=formatC(Cantidad, big.mark = ".", decimal.mark = ",")), family="font", color="white", size=7) +
  labs(title="",
       x="Trimestre-Año", y="Cantidad") +
  scale_y_continuous(limits = c(0,10000), labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="d")) +
  theme_light() +
  scale_alpha_continuous(range=c(0.5, 1)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
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
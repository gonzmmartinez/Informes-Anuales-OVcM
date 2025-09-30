# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(googlesheets4)
library(purrr)
library(readxl)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Distribucion_preservativos.csv")) %>%
  select(Año = ejercicio_presupuestario,
         Cantidad = SUM.de.ejecutado_acumulado_trim1) %>%
  filter(Año != 2025) %>%
  mutate(Cantidad = parse_number(Cantidad, locale=locale(grouping_mark = ".")))

Data <- Raw

Colores <- c("Salta" = "#72BAA9",
             "Tasa nacional" = "#f78154",
             "Resto de provincias" = "#D5E7B5")

# Grafico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad)) +
  geom_col(aes(fill=Cantidad)) +
  geom_text(aes(label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"), size=Cantidad),
            color="grey10", family="font", fontface="bold", nudge_y=2000000, vjust=0) +
  scale_x_continuous(breaks = seq(from=2014, to=2024, by=1),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg")) +
  scale_y_continuous(labels = function(z) formatC(z, format = "fg", big.mark = ".", decimal.mark = ","),
                     expand = c(0,0), breaks = seq(from=0, to=60000000, by=10000000)) +
  scale_fill_gradient2(high="#72BAA9", mid="#72BAA9", low="#f78154", midpoint=40000000) +
  scale_size_continuous(range=c(2.5, 3.5)) +
  theme_light() +
  coord_cartesian(xlim=c(2014-0.2, 2024+0.2), ylim=c(-2000000,65000000), clip="off") +
  labs(y="Cantidad de preservativos distribuídos") +
  theme(text=element_text(family="font"),
        legend.position="none",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid = element_line(color="grey95", linewidth = 0.5),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font", lineheight = 1),
        axis.title.y = element_text(size=15, family="font"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=5)


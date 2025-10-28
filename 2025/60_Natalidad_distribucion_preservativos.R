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
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Prespuesto/Presupuesto.csv")) %>%
  filter(Categoria == "Distribución de Preservativos") %>%
  select(-Categoria)

Data <- Raw %>%
  pivot_longer(cols = c(Ejecutado, Vigente),
               names_to = "Tipo",
               values_to = "Cantidad") %>%
  mutate(Tipo = ifelse(Tipo == "Vigente", "Cantidad programada", "Cantidad ejecutada")) %>%
  mutate(Tipo = factor(Tipo, levels=c("Cantidad ejecutada", "Cantidad programada")))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Cantidad ejecutada" = "#852f8c",
             "Cantidad programada" = "grey85")
Colores_text <- c("Cantidad ejecutada" = "white",
                  "Cantidad programada" = "grey50")

# Grafico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad)) +
  geom_col(data = subset(Data, Tipo=="Cantidad programada"),
           aes(fill=Tipo), position="identity") +
  geom_col(data = subset(Data, Tipo=="Cantidad ejecutada"),
           aes(fill=Tipo), position="identity") +
  geom_text(data = subset(Data, Tipo=="Cantidad programada"),
            aes(label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"), color=Tipo),
            family="font", fontface="bold", nudge_y=3000000, vjust=0.5, show.legend = FALSE, size=2.5) +
  geom_text(data = subset(Data, Tipo=="Cantidad ejecutada"),
            aes(label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"), color=Tipo),
            family="font", fontface="bold", nudge_y=-3000000, vjust=0.5, show.legend = FALSE, size=2.5) +
  scale_x_continuous(breaks = seq(from=2014, to=2024, by=1),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg")) +
  scale_y_continuous(labels = function(z) formatC(z, format = "fg", big.mark = ".", decimal.mark = ","),
                     expand = c(0,0), breaks = seq(from=0, to=120000000, by=20000000)) +
  scale_fill_manual(values=Colores) +
  scale_color_manual(values=Colores_text) +
  theme_light() +
  coord_cartesian(xlim=c(2014-0.2, 2024+0.2), ylim=c(-5000000,125000000), clip="off") +
  labs(y="Cantidad") +
  theme(text=element_text(family="font_sans"),
        legend.position="bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(color="grey95", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_sans", size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_sans", size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_sans", size=15, lineheight = 1),
        axis.title.y = element_text(size=15, family="font_sans"),
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


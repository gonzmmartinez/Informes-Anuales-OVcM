# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%nin%` = Negate(`%in%`)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Cargar datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Lista_elecciones_2025.csv"))

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Mujeres" = "#ff621d",
             "Varones" = "#852f8c")

# Modificar datos
Data <- Raw %>%
  arrange(Cargo, desc(Titularidad)) %>%
  distinct(DNI, .keep_all = TRUE)

# Agregar datos
Resultados <- Data %>%
  mutate(Orden = ifelse(is.na(Orden), "1", Orden)) %>%
  mutate(Orden = factor(as.character(Orden), levels=as.character(21:1))) %>%
  group_by(Orden, Género) %>% 
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = (100 * Cantidad)/sum(Cantidad)) %>%
  ungroup %>%
  filter(Orden %in% as.character(1:10)) %>%
  mutate(Género = ifelse(Género == "Mujer", "Mujeres", "Varones"))

# Gráfico
grafico <- ggplot(Resultados, aes(x=Porcentaje, y=Orden, fill=Género)) +
  geom_col(position = "stack") +
  geom_vline(xintercept=50, linetype=2, color="grey") +
  geom_text(aes(label=paste0(round(Porcentaje,1), "%")), family="font_sans", color="white", size=4,
            position = position_stack(vjust = .5), vjust=0, fontface="bold") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", decimal.mark = ",", format="g")), family="font", color="white", size=4,
            position = position_stack(vjust = .5), vjust=1.5) +
  scale_fill_manual(name = "Género", values = Colores) +
  scale_x_continuous(labels = function(z) paste0(z, "%")) +
  labs(y="Orden en la lista") +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_text(family="font_sans", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font_sans"),
        axis.text.y = element_text(size=15, family="font_sans", hjust=1),
        axis.text.x = element_text(size=12, family="font_sans"),
        axis.title.y = element_text(size=20, family="font_sans", angle = 90, margin=margin(r=5)),
        axis.ticks = element_blank(),
        legend.title = element_text(family="font_serif", size=12, margin=margin(b=10)),
        legend.text = element_text(size=15, family="font_sans"),
        legend.box.margin = margin(t=5,b=5,l=0,r=0),
        legend.key.spacing.y = unit(0.5, "cm"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(linewidth=0.5, color="grey95"),
        plot.margin = margin(t=10, b=10, l=10, r=10),
        strip.text = element_text(size=20, family="font", face="bold"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)
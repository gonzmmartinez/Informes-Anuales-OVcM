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

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Mujeres" = "#ff621d",
             "Varones" = "#852f8c")

# Modificar datos
Data <- Raw

# Agregar datos
Resultados <- Data %>%
  group_by(Departamento, Género) %>% 
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = (100 * Cantidad)/sum(Cantidad)) %>%
  ungroup

Orden <- (Resultados %>%
  filter(Género == "Mujer") %>%
  arrange(desc(Porcentaje)))$Departamento
Orden <- append(Orden, "Guachipas")

Resultados <- Resultados %>%
  mutate(Departamento = factor(Departamento, levels=c(Orden))) %>%
  mutate(Género = ifelse(Género == "Mujer", "Mujeres", "Varones"))

# Gráfico
grafico <- ggplot(Resultados, aes(x=Porcentaje, y=Departamento, fill=Género)) +
  geom_col(position = "stack") +
  geom_vline(xintercept=50, linetype=2, color="grey") +
  geom_text(aes(label=paste0(round(Porcentaje,1), "%")), family="font_sans", color="white", size=4,
            position = position_stack(vjust = .5), vjust=0, fontface="bold") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", decimal.mark = ",", format="g")),
            family="font", color="white", size=3,
            position = position_stack(vjust = .5), vjust=1.5) +
  scale_fill_manual(name = "Género", values = Colores) +
  scale_x_continuous(labels = function(z) paste0(z, "%")) +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        plot.title =  element_blank(),
        plot.subtitle = element_blank(),
        axis.text.y = element_text(size=15, family="font_sans", margin=margin(r=5), hjust=1),
        axis.text.x = element_text(size=12, family="font_sans"),
        axis.title = element_text(size=15, family="font_sans"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(0.5, "cm"),
        plot.margin = margin(t=10, b=10, l=10, r=10),
        plot.background = element_rect(fill = "white", colour = NA),
        strip.background = element_rect(fill="#cbc2ce", color=NA),
        strip.text = element_text(color="black", size=15, family="font_serif", face="bold", margin=margin(t=10, b=10)))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=15, height=15)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=15, height=15)
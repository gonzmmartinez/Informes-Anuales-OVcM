# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%nin%` = Negate(`%in%`)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Cargar datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Lista_elecciones_2025.csv"))

# Colores
Paleta <- c("#2a1f82", "#3f3af2", "#a4fffd", "#e3e2ef", "#27391C", "#1F7D53")
Colores <- c("Mujeres" <- "#1F7D53",
             "Varones" <- "#2a1f82")

# Modificar datos
Data <- Raw %>%
  arrange(Cargo, desc(Titularidad)) %>%
  distinct(DNI, .keep_all = TRUE)

# Agregar datos
Resultados <- Data %>%
  group_by(Cargo, Género) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = (100 * Cantidad)/sum(Cantidad)) %>%
  ungroup %>%
  mutate(Cargo = case_when(Cargo == "Convencional Municipal" ~ "Convencionales Municipales",
                           Cargo == "Concejal" ~ "Concejales/as",
                           Cargo == "Diputado" ~ "Diputados/as",
                           Cargo == "Senador" ~ "Senadores/as")) %>%
  mutate(Cargo = factor(Cargo, levels=c("Convencionales Municipales", "Concejales/as", "Diputados/as", "Senadores/as")))

# Gráfico
grafico <- ggplot(Resultados, aes(x=Porcentaje, y=Cargo, fill=Género)) +
  geom_col(position = "stack") +
  geom_vline(xintercept=50, linetype=2, color="black") +
  geom_text(aes(label=paste0(round(Porcentaje,1), "%")), family="font", color="white", size=4,
            position = position_stack(vjust = .5), vjust=0, fontface="bold") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", decimal.mark = ",", format="g")), family="font", color="white", size=4,
            position = position_stack(vjust = .5), vjust=1.5) +
  scale_fill_manual(name = "Género", values = Colores) +
  scale_y_discrete(labels = function(z) str_wrap(z, width=10)) +
  scale_x_continuous(labels = function(z) paste0(z, "%")) +
  theme_void() +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        axis.text.y = element_text(size=15, family="font"),
        axis.text.x = element_text(size=12, family="font"),
        axis.ticks = element_blank(),
        legend.title = element_text(family="font", size=12, margin=margin(b=10)),
        legend.text = element_text(size=12),
        legend.box.margin = margin(t=5,b=5,l=0,r=0),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=10, b=10, l=10, r=10),
        plot.background = element_rect(fill = "white", colour = NA),
        strip.text = element_text(size=20, family="font", face="bold"))


# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=5)
ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=5)
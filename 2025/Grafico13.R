# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%nin%` = Negate(`%in%`)

# Fuentes
library(showtext)
font_add_google("Poppins", "font")
showtext_auto()

# Cargar datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2025.csv"))

# Modificar datos
Data <- Raw %>%
  mutate(Rango_etario_agresor = str_trim(str_remove_all(Rango_etario_agresor, "años"))) %>%
  group_by(Rango_etario_agresor, Rango_agr_ord) %>%
  summarise(Cantidad = n())

Tabla <- data.frame(
  Rango_etario_agresor = c("5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "75-79", "80-84", "85-89", "90-94", "Sin dato"),
  Rango_agr_ord = 1:18)

Data <- Tabla %>%
  left_join(Data, by="Rango_etario_agresor") %>%
  select(-Rango_agr_ord.y) %>%
  mutate(Cantidad = ifelse(is.na(Cantidad), 0, Cantidad)) %>%
  rename(Rango_ord = "Rango_agr_ord.x")

# Definir colores
Colores <- c("Mujer" = "#8cb369",
             "Varón" = "#f4a259",
             "Mujer trans" = "#bc4b51",
             "Otro" = "#735751")

# Gr?fico
grafico <- ggplot(Data, aes(x=reorder(Rango_etario_agresor, Rango_ord), y=Cantidad)) +
  geom_col(aes(alpha = Cantidad), fill=c(rep("#bc4b51",17), "grey90")) +
  geom_text(aes(y=ifelse(Cantidad>10,Cantidad/2,Cantidad+5), label=Cantidad), family="font", color="black", size=5) +
  labs(title="",
       x="Rango etario de la persona agresora", y="Cantidad") +
  theme_light() +
  scale_alpha_continuous(range=c(0.5, 1)) +
  theme(text=element_text(family="font"), legend.position="none",
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/"),
       plot=grafico, dpi=100, width=12, height=6)
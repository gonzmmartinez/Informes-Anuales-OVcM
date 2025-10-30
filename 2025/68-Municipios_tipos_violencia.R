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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2025.csv"))

# Modificar datos
Data <- Raw %>%
  mutate(Tipo = ifelse(Tipo == "Económica/Patrimonial", "Económica/ Patrimonial", Tipo)) %>%
  filter(Tipo != "Sin dato") %>%
  group_by(Tipo) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  arrange(desc(Porcentaje)) %>%
  mutate(Ord = row_number())

Sin_dato <- Raw %>%
  filter(Tipo == "Sin dato")

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Mujer" = "#8cb369",
             "Varón" = "#f4a259",
             "Mujer trans" = "#bc4b51",
             "Otro" = "#735751")

# Gr?fico
grafico <- ggplot(Data, aes(x=Porcentaje, y=reorder(Tipo, -Ord))) +
  geom_col(aes(alpha = Cantidad), fill="#a782ec") +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), format="fg", big.mark = ".", decimal.mark = ","), "%")),
            nudge_x = 1, hjust=0,family="font_sans", color="black", size=4, vjust=-0.5, fontface="bold") +
  geom_text(aes(label=Cantidad), nudge_x = 1, hjust=0,
            family="font_sans", color="black", size=3, vjust=1.25) +
  annotate(geom="text", y=0.75, x=40, label=paste0("Sin dato: ", nrow(Sin_dato)), size=4,
           family="font_sans", fontface="italic", color="darkgrey") +
  labs(title="",
       y="Tipo de violencia", x="Porcentaje") +
  theme_light() +
  scale_alpha_continuous(range=c(0.5, 1)) +
  scale_y_discrete(labels = function(z) str_wrap(z, width=10)) +
  scale_x_continuous(labels = function(z) paste0(round(z,2), "%"), limits=c(0,45)) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=12.5),
        axis.title.y = element_text(size=12.5))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=5)
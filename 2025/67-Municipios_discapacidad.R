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
  mutate(Discapacidad = case_when(Discapacidad == "Sí" ~ "Si",
                                  Discapacidad == "Si" ~ "Si",
                                  Discapacidad == "No" ~ "No",
                                  Discapacidad == "Sin dato" ~ "Sin dato")) %>%
  filter(Discapacidad != "Sin dato") %>%
  mutate(Discapacidad = factor(Discapacidad, levels=c("Si", "No"))) %>%
  group_by(Discapacidad) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Label = ifelse(Porcentaje < 3,
                        paste0("<span style='font-size:10pt'>**",
                               formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=","),
                               "%**</span><br><span style='font-size:7.5pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")),
                        paste0("<span style='font-size:12.5pt'>**",
                               formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=","),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",","</span>")))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2)

Sin_dato <- paste0(paste0("<span style='font-size:12.5pt'>",
                       "_Sin dato: **",
                       formatC(nrow(Raw %>% filter(Discapacidad == "Sin dato")), big.mark = ".", decimal.mark = ","),
                       "**_</span>"))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Si" = "#ff621d",
             "No" = "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.75, fill=Discapacidad)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Sin_dato, size=9,
                color = "darkgrey",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = ifelse(Porcentaje <= 3, 4.4, 3.5), y=ymid, label=Label),
                color = "white",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("¿Es una persona con discapacidad?", 15), values = Colores) +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_text(family="font_serif", size=12, margin=margin(b=10)),
        legend.text = element_text(family="font_sans", size=12),
        legend.box.margin = margin(t=5,b=5,l=-50,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-50, b=-50, l=-50, r=-50),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=4.5)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=4.5)
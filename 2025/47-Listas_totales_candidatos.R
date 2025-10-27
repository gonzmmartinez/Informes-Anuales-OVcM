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
  group_by(Género) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = (100 * Cantidad) / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Género = ifelse(Género == "Mujer", "Mujeres", "Varones")) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        paste0("<span style='font-size:10pt'>**",
                               formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"),
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ","),
                               "</span>"))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Total <- paste0(paste0("<span style='font-size:20pt'>",
                       "Total",
                       "</span><br><span style='font-size:30pt'>**",
                       formatC(sum(Resultados$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span>"))

# Gráfico
grafico <- ggplot(Resultados, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.75, fill=Género)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=9,
                color = "black",
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = ifelse(Porcentaje <= 5, 4.4, 3.4), y=ymid, label=Label),
                color = "white",
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name = "Género", values = Colores) +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_text(family="font_serif", size=12, margin=margin(b=10)),
        legend.text = element_text(size=15, family="font_sans"),
        legend.box.margin = margin(t=5,b=5,l=-40,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-50, b=-50, l=-50, r=-50),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG"),
       plot=grafico, dpi=100, width=6, height=4.5)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF"),
       plot=grafico, dpi=72, width=6, height=4.5)
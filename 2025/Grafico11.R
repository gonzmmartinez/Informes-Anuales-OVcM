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
  mutate(Medidas = case_when(Medidas == "Sí" ~ "Si",
                             Medidas == "Si" ~ "Si",
                             Medidas == "No" ~ "No",
                             Medidas == "Sin dato" ~ "Sin dato")) %>%
  filter(Medidas != "Sin dato") %>%
  mutate(Medidas = factor(Medidas, levels=c("Si", "No"))) %>%
  group_by(Medidas) %>%
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
                       formatC(nrow(Raw %>% filter(Medidas == "Sin dato")), big.mark = ".", decimal.mark = ","),
                       "**_</span>"))

# Definir colores
Colores <- c("Si" = "#5B8E7D",
             "No" = "#BC4B51")

# Gráfico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.75, fill=Medidas)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Sin_dato, size=9,
                color = "darkgrey",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = ifelse(Porcentaje <= 3, 4.4, 3.5), y=ymid, label=Label),
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Existencia de medidas de protección", 15), values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_text(family="font", size=17.5, margin=margin(b=10)),
        legend.text = element_text(size=15),
        legend.box.margin = margin(t=5,b=5,l=-50,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-50, b=-50, l=-50, r=-50),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/"),
       plot=grafico, dpi=100, width=7, height=4.5)
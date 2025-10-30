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
  filter(Género_agresor != "Sin dato") %>%
  mutate(Género_agresor = ifelse(Género_agresor == "Mujer trans", "Otro", Género_agresor)) %>%
  mutate(Género_agresor = factor(Género_agresor, levels=c("Mujer", "Varón", "Otro"))) %>%
  group_by(Género_agresor) %>%
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
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Total <- paste0(paste0("<span style='font-size:17.5pt'>",
                       "Total",
                       "</span><br><span style='font-size:35pt'>**",
                       formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ","),
                       "**</span><br><span style='font-size:12.5pt; color:#A9A9A9'>",
                       "_Sin dato: **",
                       formatC(nrow(Raw %>% filter(Género_agresor == "Sin dato")), big.mark = ".", decimal.mark = ","),
                       "**_</span>"))

# Definir colores
Colores <- c("Mujer" = "#8cb369",
             "Varón" = "#f4a259",
             "Otro" = "#735751")

# Gráfico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Género_agresor)) +
  geom_rect() +
  geom_richtext(y=0, x=1.5,
                label=Total, size=9,
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  geom_richtext(aes(x = ifelse(Porcentaje <= 3, 4.4, 3.5), y=ymid, label=Label),
                color = "black",
                label.color = NA, family="font",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Género de la persona agresora", width=15), values = Colores) +
  theme(text=element_text(family="font"),
        legend.position = "right",
        plot.title = element_text(family="font", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(size=12, family="font"),
        legend.title = element_text(family="font", size=12, margin=margin(b=10)),
        legend.text = element_text(size=15),
        legend.box.margin = margin(t=5,b=5,l=-40,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-20, b=-50, l=-50, r=-50),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/"),
       plot=grafico, dpi=100, width=6, height=4.5)
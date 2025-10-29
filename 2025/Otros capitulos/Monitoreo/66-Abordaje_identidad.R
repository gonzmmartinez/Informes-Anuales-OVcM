# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Crear datos
Data <- data.frame(Categoria = c("Desconoce cómo se aborda", "Vemos caso a caso",
                                 "No hay/hubo estudiantes trans", "No se aborda", "Otra"),
                   Cantidad = c(3, 8, 1, 1, 1)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        "",
                        paste0("<span style='font-size:25pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:20pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, as.character(Categoria),
                          paste0(Categoria, " (", formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ","), "%)"))) %>%
  mutate(Categoria = factor(Categoria, levels=c("Desconoce cómo se aborda", "Vemos caso a caso", "No hay/hubo estudiantes trans",
                                                "No se aborda", "Otra")))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Desconoce cómo se aborda" = "#2b42a0",
             "Vemos caso a caso" = "#d3335e",
             "No hay/hubo estudiantes trans" = "#206170",
             "No se aborda" = "#ff621d",
             "Otra" = "#cbc2ce")

# Gr?fico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=Categoria)) +
  geom_rect() +
  geom_richtext(aes(x = ifelse(Porcentaje <= 5, 4.4, 3.3), y=ymid, label=Label),
                color = "white", lineheight = ifelse(Data$Porcentaje <= 10, 1.5, 2),
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  labs(title=str_wrap("¿Cómo abordan el tema de la identidad de género en el colegio?",
                      width=70),
       subtitle=str_wrap("Personal docente y administrativo que NO vieron los afiches, n = 14", width=70)) +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(values = Colores,
                    labels = function(z) str_wrap(Data$Leyenda[match(z, Data$Categoria)], width = 20)) +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_text(size=15, family="font_serif", margin=margin(b=5), face="italic", hjust=0.5, color="grey30"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=12, family="font_serif", margin=margin(b=-60), hjust=0.5, color="grey30"),
        legend.title = element_blank(),
        legend.text = element_text(size=15, family="font_sans"),
        legend.box.margin = margin(t=5,b=5,l=-40,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=10, r=-50, b=-50, l=-70),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=6)
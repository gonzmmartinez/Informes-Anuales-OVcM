# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Crear datos
Data1 <- data.frame(Categoria = factor(c("Sí", "No"), levels=c("Sí", "No")),
                   Cantidad = c(40, 1)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        "",
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, as.character(Categoria),
                          paste0(Categoria, " (", formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ","), "%)")))

Data2 <- data.frame(Categoria = factor(c("Sí", "No"), levels=c("Sí", "No")),
                   Cantidad = c(33, 8)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        "",
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, as.character(Categoria),
                          paste0(Categoria, " (", formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ","), "%)")))

# Colores
Paleta2 <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
             "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Sí" = "#d3335e",
             "No" = "#cbc2ce")
Colores2 <- c("Sí" = "#ff621d",
             "No" = "#cbc2ce")

# Gráfico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=Categoria)) +
  geom_rect() +
  geom_richtext(aes(x = 3.25, y=ymid, label=Label), size=3,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores,
                    labels = function(z) str_wrap(Data1$Leyenda[match(z, Data1$Categoria)], width = 2)) +
  labs(title=str_wrap("¿Sabía que existe la Ley de Identidad de Género N.º 26.743?",
                      width=50),
       subtitle=str_wrap("Total de personas que respondieron la encuesta, n = 41", width=40)) +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_text(size=10, family="font_serif", margin=margin(b=10), face="italic", hjust=0.5, color="grey30"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=8, family="font_serif", margin=margin(b=0), hjust=0.5, color="grey30"),
        legend.title = element_blank(),
        legend.text = element_text(size=10, family="font_sans"),
        legend.box.margin = margin(t=5,b=5,l=5,r=-5),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=10, r=0, b=0, l=0),
        plot.background = element_rect(fill = "white", colour = NA))

# Gr?fico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.5, fill=Categoria)) +
  geom_rect() +
  geom_richtext(aes(x = 3.25, y=ymid, label=Label), size=3,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores2,
                    labels = function(z) str_wrap(Data2$Leyenda[match(z, Data2$Categoria)], width = 20)) +
  labs(title=str_wrap("¿Sabía que existe un Protocolo de cambio de identidad de género en espacios educativos? (Res. 635/21)",
                      width=60),
       subtitle=str_wrap("Total de personas que respondieron la encuesta, n = 41", width=40)) +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_text(size=10, family="font_serif", margin=margin(b=10), face="italic", hjust=0.5, color="grey30"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size=8, family="font_serif", margin=margin(b=0), hjust=0.5, color="grey30"),
        legend.title = element_blank(),
        legend.text = element_text(size=10, family="font_sans"),
        legend.box.margin = margin(t=5,b=5,l=5,r=-5),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=10, r=20, b=0, l=0),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- plot_grid(grafico1, grafico2, ncol=2, vjust = 0, rel_widths = c(1,1), rel_heights = c(1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=3.5)
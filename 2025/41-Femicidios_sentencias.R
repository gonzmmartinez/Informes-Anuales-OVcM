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

# Leer datos
Raw <- data.frame(Caratula = c("Femicidio íntimo", "Femicidio no íntimo", "Homicidio agravado por el vínculo",
                               "Homicidio criminis causa", "Homicidio preterintencional"),
                  Cantidad = c(27,8,2,1,1))

Data <- Raw %>%
  mutate(Caratula = factor(Caratula, levels = Raw$Caratula)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = ifelse(Porcentaje < 10,
                        paste0("<span style='font-size:12pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"),
                        paste0("<span style='font-size:15pt'>**",
                               formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                               "%**</span><br><span style='font-size:10pt'>",
                               formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                               "</span>"))) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Total
Total <- paste0("<span style='font-size:20pt'>Total</span><br>",
                "**", formatC(sum(Data$Cantidad), big.mark = ".", decimal.mark = ",", format = "fg"),
                "**")

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Definir colores
Colores <- c("Femicidio íntimo" = "#f93e35",
             "Femicidio no íntimo" = "#206170",
             "Homicidio agravado por el vínculo" = "#ff9d27",
             "Homicidio criminis causa" = "#d3335e",
             "Homicidio preterintencional" = "#2b42a0")

# Gr?fico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Caratula)) +
  geom_rect() +
  geom_textbox(x = 1.5, y = 0, label = Total, hjust = 0.5,
               halign = 0.5, fill = NA, size=20, box.color=NA,
               family = "font_sans", lineheight = 0.25) +
  geom_richtext(aes(x = ifelse(Porcentaje <= 5, 4.4, 3.5), y=ymid, label=Label),
                color = ifelse(Data$Porcentaje <= 5, "black", "white"),
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4.5)) +
  theme_void() +
  scale_fill_manual(name = "Tipo",values = Colores, labels = function(z) str_wrap(z, width=20)) +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.title = element_text(family="font_serif", size=12, margin=margin(b=15)),
        legend.text = element_text(size=15, family="font_sans"),
        legend.box.margin = margin(t=5,b=5,l=-40,r=40),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.margin = margin(t=-75, r=0, b=-100, l=-30),
        plot.background = element_rect(fill = "white", colour = NA))

# Layout
grafico <- grafico +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=4.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=4.5)
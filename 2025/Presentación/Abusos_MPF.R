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

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/14m3Y-OdpPdvq0QLcFDZ7c0diTdoExJOHmOaY70qaeV8/edit?usp=sharing",
                  sheet = "Abuso")

Data1 <- Raw %>%
  filter(Año == 2025) %>%
  group_by(Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                       levels = c("A caratular - Delitos sexuales",
                                  "Abuso sexual con acceso carnal",
                                  "Abuso sexual gravemente ultrajante",
                                  "Abuso sexual simple",
                                  "Grooming",
                                  "A caratular - delitos sexuales"))) %>%
  arrange(Tipo) %>%
  mutate(Label = paste0("<span style='font-size:10pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:6pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(Label = ifelse(Porcentaje >= 5, Label, "")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Leyenda = ifelse(Porcentaje >= 10, as.character(Tipo),
                          paste0(Tipo, " (", formatC(round(Porcentaje,1), big.mark=".", decimal.mark = ","), "%)")))


# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Abuso sexual con acceso carnal" = "#ff9d27",
             "Abuso sexual gravemente ultrajante" = "#ff621d",
             "Abuso sexual simple" = "#f93e35",
             "Grooming" = "#2b42a0",
             "A caratular - delitos sexuales" = "#a782ec")

# Total
Total1 <- paste0( "<span style='font-size:15pt'>Total</span><br>",
                  "**", formatC(sum(Data1$Cantidad), big.mark = ".", decimal.mark = ",", format = "fg"),
                  "**")

# Gráfico1
grafico <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_textbox(x = 1.5, y = 0, label = Total1, hjust = 0.5,
               halign = 0.5, fill = NA, size=8, box.color=NA,
               family = "font_sans", lineheight = 0.75) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=3,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Tipo de abuso", width=20),
                    values = Colores,
                    labels=str_wrap(Data1$Leyenda, 25)) +
  labs(title="2.025",
       subtitle = str_wrap("primer semestre", 20)) +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_text(family="font_serif", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_serif", size=10, face="italic", hjust=0.5),
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=10, family="font_sans"),
        legend.key.spacing.y = unit(0.25, "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=5, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=5, height=3.5)
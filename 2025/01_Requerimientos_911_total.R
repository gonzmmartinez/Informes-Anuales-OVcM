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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                   sheet = "Mes")

Data1 <- Raw %>%
  filter(Año == 2025, Tipo != "Abuso sexual") %>%
  group_by(Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                         levels = c("Violencia de género", "Violencia familiar en curso", "Violencia familiar histórica"))) %>%
  mutate(Label = paste0("<span style='font-size:10pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                        "%**</span><br><span style='font-size:6pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Data2 <- Raw %>%
  filter(Año == 2024, Tipo != "Abuso sexual") %>%
  group_by(Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                       levels = c("Violencia de género", "Violencia familiar en curso", "Violencia familiar histórica"))) %>%
  mutate(Label = paste0("<span style='font-size:8pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                        "%**</span><br><span style='font-size:4pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Definir colores
Colores <- c("Violencia de género" = "#f2904c",
             "Violencia familiar en curso" = "#ec6489",
             "Violencia familiar histórica" = "#a5549c")

# Total
Total1 <- paste0( "<span style='font-size:15pt'>Total</span><br>",
                  "**", formatC(sum(Data1$Cantidad), big.mark = ".", decimal.mark = ",", format = "fg"),
                  "**")

Total2 <- paste0( "<span style='font-size:15pt'>Total</span><br>",
                  "**", formatC(sum(Data2$Cantidad), big.mark = ".", decimal.mark = ",", format = "fg"),
                  "**")

# Gr?fico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_textbox(x = 1.5, y = 0, label = Total1, hjust = 0.5,
               halign = 0.5, fill = NA, size=8, box.color=NA,
               family = "font_sans", lineheight = 0.75) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=3,
                color = "black", hjust=0.5, lineheight=1,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Situación motivo del requerimiento", width=20),
                    values = Colores, labels = function(z) str_wrap(z, width=20)) +
  labs(title="2.025",
       subtitle = "enero-junio") +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_text(family="font_serif", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_serif", size=10, face="italic", hjust=0.5),
        legend.title = element_text(size=10, family="font_sans"),
        legend.text = element_text(size=12),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.y = unit(0.5, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Gr?fico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_textbox(x = 1.5, y = 0, label = Total2, hjust = 0.5,
               halign = 0.5, fill = NA, size=6, box.color=NA,
               family = "font_sans", lineheight = 1) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=3,
                color = "black", hjust=0.5, lineheight=1,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  labs(title="2.024",
       subtitle="enero-diciembre") +
  theme(text=element_text(family="font_sans"),
        legend.position = "none",
        plot.title = element_text(family="font_serif", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_serif", size=10, face="italic", hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5))

# Layout
grafico <- plot_grid(grafico2, grafico1, ncol=2,
                     rel_widths = c(1,2)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=3.5)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=3.5)
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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Persona_denunciada")

# Modificar datos
Data1 <- Raw %>%
  filter(Año == 2025) %>%
  group_by(Año, Género) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = paste0("<span style='font-size:20pt'>**",
                        formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ",", format="fg"),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

Data2 <- Raw %>%
  filter(Año == 2024) %>%
  group_by(Año, Género) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",
                        formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup()

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Mujeres" = "#ff621d",
             "Varones" = "#852f8c")

# Gráfico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Género)) +
  geom_rect() +
  geom_richtext(aes(x = 3, y=ymid, label=Label),
                color = "white", hjust=0.5, lineheight=1.25,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(name = "Género",
                    values = Colores) +
  labs(title="2.025",
       subtitle = "primer semestre") +
  theme(text=element_text(family="font_sans"),
        legend.position = "right",
        plot.title = element_text(family="font_serif", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_serif", size=10, face="italic", hjust=0.5),
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=10, family="font_sans"),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Gráfico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=2.25, fill=Género)) +
  geom_rect() +
  geom_richtext(aes(x = 3, y=ymid, label=Label),
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_sans",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  labs(title="2.024",
       subtitle="Todo el año") +
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
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=3.5)
# Limpiar todo
rm(list = ls())

# Funciones
`%nin%` <- function(x, table) !(x %in% table)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

# Fuentes
library(sysfonts)
library(showtext)

dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Años
Año_1 <- 2026
Año_2 <- 2025

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Ingresadas") %>%
  filter(Tipo != "No configura VFG")

Data1 <- Raw %>%
  filter(Año == Año_1, Tipo %in% c("Género", "Familiar")) %>%
  group_by(Tipo) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                         levels = c("Género", "Familiar"))) %>%
  arrange(Tipo) %>%
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
  filter(Año == Año_2, Tipo %in% c("Género", "Familiar")) %>%
  group_by(Tipo) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                       levels = c("Género", "Familiar"))) %>%
  arrange(Tipo) %>%
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

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Género" = "#6963aa",
             "Familiar" = "#c72a29")

# Total
Total1 <- paste0( "<span style='font-size:15pt'>Total</span><br>",
                  "**", formatC(sum(Data1$Cantidad), big.mark = ".", decimal.mark = ",", format = "fg"),
                  "**")

Total2 <- paste0( "<span style='font-size:15pt'>Total</span><br>",
                  "**", formatC(sum(Data2$Cantidad), big.mark = ".", decimal.mark = ",", format = "fg"),
                  "**")

# Gráfico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_textbox(x = 1.5, y = 0, label = Total1, hjust = 0.5,
               halign = 0.5, fill = NA, size=8, box.color=NA,
               family = "font_title", lineheight = 0.75) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=3,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores,
                    name = str_wrap("Situación motivo de la denuncia" ,width=20),
                    labels = str_wrap(c("Violencia de género", "Violencia familiar"), width=20)) +
  labs(title="2026",
       subtitle = "primer semestre") +
  theme(text=element_text(family="font_body"),
        legend.position = "right",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_title", size=10, hjust=0.5),
        legend.title = element_text(size=10, family="font_title", face="bold"),
        legend.text = element_text(size=10, family="font_title"),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Gr?fico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_textbox(x = 1.5, y = 0, label = Total2, hjust = 0.5,
               halign = 0.5, fill = NA, size=6, box.color=NA,
               family = "font_title", lineheight = 1) +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=3,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(1.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  labs(title="2025",
       subtitle="enero-diciembre") +
  theme(text=element_text(family="font_body"),
        legend.position = "none",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_title", size=10, hjust=0.5),
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

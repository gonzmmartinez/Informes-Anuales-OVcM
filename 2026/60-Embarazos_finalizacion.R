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
library(tidyr)
library(tibble)

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
Año_1 <- 2025
Año_2 <- 2024

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1cl0-rAT-ARDgQQQIjCApJ_hXtrWXjImAGrFnmzMDukw/edit?usp=sharing",
                  sheet = "Finalizacion_embarazo",
                  range = "A3:C9") %>%
  pivot_longer(cols = c(`2024`, `2025`),
               names_to = "Año",
               values_to = "Cantidad") %>%
  rename(Tipo = "Modalidad") %>%
  mutate(Tipo = ifelse(Tipo %in% c("Cesárea", "Parto vaginal"), "Parto vaginal o cesárea", Tipo)) %>%
  group_by(Año, Tipo) %>%
  summarise(Cantidad = sum(Cantidad))

Data1 <- Raw %>%
  filter(Tipo != "Sin dato") %>%
  filter(Año == Año_1) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                       levels = c("Parto vaginal o cesárea", "IVE/ILE",
                       "Aborto espontáneo", "Otro (HMyR, embarazo anembrionado, etc)"))) %>%
  arrange(Tipo) %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(Label = ifelse(Porcentaje >= 5, Label, "")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2)

Data2 <- Raw %>%
  filter(Tipo != "Sin dato") %>%
  filter(Año == Año_2) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo,
                       levels = c("Parto vaginal o cesárea", "IVE/ILE",
                                  "Aborto espontáneo", "Otro (HMyR, embarazo anembrionado, etc)"))) %>%
  arrange(Tipo) %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg"),
                        "</span>")) %>%
  mutate(Label = ifelse(Porcentaje >= 5, Label, "")) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) 

Sin_dato_1 <- Raw %>%
  filter(Tipo == "Sin dato") %>%
  filter(Año == Año_1) %>%
  mutate(Texto = paste0("Sin dato: ", Cantidad)) %>%
  pull(Texto)

Sin_dato_2 <- Raw %>%
  filter(Tipo == "Sin dato") %>%
  filter(Año == Año_2) %>%
  mutate(Texto = paste0("Sin dato: ", Cantidad)) %>%
  pull(Texto)

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Parto vaginal o cesárea" = "#6963aa",
             "Cesárea" = "#119ca0",
             "IVE/ILE" = "#1e7b34",
             "Aborto espontáneo" = "#ec6230",
             "Otro (HMyR, embarazo anembrionado, etc)" = "#cbc2ce")

Leyenda <- Data1 %>%
  ungroup() %>%
  mutate(Leyenda = ifelse(Porcentaje >= 5, as.character(Tipo), paste0(Tipo, " (", formatC(round(Porcentaje, 1), format = "fg", decimal.mark = ","), "%)")),
         Leyenda = str_wrap(Leyenda, 25)) %>%
  select(Tipo, Leyenda) %>%
  deframe()

# Gráfico1
grafico1 <- ggplot(Data1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=5,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(2.5, 4)) +
  theme_void() +
  scale_fill_manual(name = str_wrap("Modalidad de finalización del embarazo", width=25),
                    values = Colores,
                    labels = Leyenda) +
  labs(title=as.character(Año_1),
       subtitle = Sin_dato_1) +
  theme(text=element_text(family="font_body"),
        legend.position = "right",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_title", size=10, hjust=0.5),
        legend.title = element_text(size=12, family="font_title", face="bold"),
        legend.text = element_text(size=12, family="font_title"),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Gráfico2
grafico2 <- ggplot(Data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Tipo)) +
  geom_rect() +
  geom_richtext(aes(x = 3.5, y=ymid, label=Label), size=5,
                color = "white", hjust=0.5, lineheight=1,
                label.color = NA, family="font_body",
                show.legend=FALSE, fill=NA) +
  coord_polar(theta="y") +
  xlim(c(2.5, 4)) +
  theme_void() +
  scale_fill_manual(values = Colores) +
  labs(title=as.character(Año_2),
       subtitle = Sin_dato_2) +
  theme(text=element_text(family="font_body"),
        legend.position = "none",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_text(family="font_title", size=10, hjust=0.5),
        legend.box.margin=margin(5,5,5,5))

# Layout
grafico <- plot_grid(grafico2, grafico1, ncol=2,
                     rel_widths = c(1,1.9)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=3.5)

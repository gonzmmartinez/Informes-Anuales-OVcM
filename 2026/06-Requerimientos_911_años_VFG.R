# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- function(x, table) !(x %in% table)

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
font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))
font_add("font_body_b",     file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

Data <- Raw %>%
  filter(Tipo %ni% c("Abuso sexual", "Abuso sexual (tentativa)")) %>%
  mutate(Tipo = case_when(Tipo == "Violencia de género" ~ "Violencia de género",
                          Tipo == "Violencia de género histórica" ~ "Violencia de género",
                          Tipo == "Violencia familiar en curso" ~ "Violencia familiar",
                          Tipo == "Violencia familiar histórica" ~ "Violencia familiar")) %>%
  mutate(Año = as.character(Año),
         Tipo = factor(Tipo, levels=c("Violencia familiar", "Violencia de género"))) %>%
  mutate(Año = as.factor(ifelse(Año %in% c("2023", "2026"), paste0(Año, "*"), Año))) %>%
  group_by(Año, Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad/sum(Cantidad))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Violencia de género" = "#6963aa",
             "Violencia familiar" = "#c72a29")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_hline(yintercept = 0.5, color="gray", linetype=2) +
  geom_col(position="fill", width=0.7) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=",", format="fg"), "%")),
            family="font_body", position = position_fill(vjust = 0.5), size=5, color="white") +
  labs(y="Porcentaje", x="Año",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_fill_manual(name = str_wrap("Motivo del requerimiento", width=20),
                    values = Colores) +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_title"),
        legend.text = element_text(size=12, family="font_subtitle"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_subtitle", face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_subtitle", size=20, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_subtitle", size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_subtitle", size=20),
        axis.title.y = element_text(family="font_subtitle", size=20))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)


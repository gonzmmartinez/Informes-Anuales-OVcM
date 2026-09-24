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

dir <- paste0(str_sub(dirname(rstudioapi::getActiveDocumentContext()$path), 1, 36),
              "/Fonts/")
font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))
showtext_auto()

# Leer datos
Raw <- read.csv(file = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Investigacion_nivel.csv"))

Data <- Raw %>%
  mutate(Ley = case_when(Ley == "Ley 26485" ~ "Ley N° 26.485",
                         Ley == "Ley 7888" ~ "Ley N° 7.888",
                         Ley == "Protocolo" ~ "Protocolo")) %>%
  group_by(Ley) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Nivel = factor(Nivel, levels=c("No la conozco", "Tengo conocimiento", "La estudié")))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("La estudié" = "#119ca0",
             "Tengo conocimiento" = "#6963aa",
             "No la conozco" = "#ec6230")

Titulos_leyenda <- c("La estudié" = str_wrap("La estudié y la conozco en detalle", 40),
                     "Tengo conocimiento" = str_wrap("Tengo conocimiento de su existencia, pero no la tengo estudiada", 40),
                     "No la conozco" = str_wrap("No la conozco", 40))

# Gráfico
grafico <- ggplot(Data, aes(x=Ley, y=Cantidad, fill=Nivel)) +
  geom_col(position="fill", width=0.8) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=",", format="fg"), "% (", Cantidad, ")")),
            family="font_body", position = position_fill(vjust = 0.5), size=5, color="white") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_fill_manual(name = "Nivel de conocimiento",
                    values = Colores,
                    labels = Titulos_leyenda) +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="right",
        legend.justification = "right",
        legend.title = element_text(size=15, family="font_title", margin=margin(b=10)),
        legend.text = element_text(size=15, family="font_subtitle"),
        legend.key.spacing.y = unit(0.33, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_subtitle", size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_subtitle", size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=7)


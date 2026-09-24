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
library(forcats)

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
Raw <- read.csv(file = paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Investigacion_cargos.csv"))

Data <- Raw %>%
  group_by(Puesto) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Género = factor(Género, levels=c("Varones", "Mixto",
                                         "Mujeres", "NC")),
         Puesto = factor(Puesto, levels = c("Presidencia", "Consejo directivo",
                                            "Dirección/gerencia general",
                                            "Gerencias intermedias"))) %>%
  mutate(Género = fct_rev(Género),
         Puesto = fct_rev(Puesto)) %>%
  mutate(Labels = ifelse(Porcentaje > 1, paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=",", format="fg"), "%\n(", Cantidad, ")"), NA))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Varones" = "#c72a29",
             "Mixto" = "#b8d6ac",
             "Mujeres" = "#1e7b34",
             "NC" = "#cbc2ce")

Titulos_leyenda <- c("Varones" = str_wrap("Solo varones", 40),
                     "Mixto" = str_wrap("Composición mixta", 40),
                     "Mujeres" = str_wrap("Solo mujeres", 40),
                     "NC" = str_wrap("No corresponde", 40))

# Gráfico
grafico <- ggplot(Data, aes(y=Puesto, x=Cantidad, fill=Género)) +
  geom_col(position="fill", width=0.8) +
  geom_text(aes(label = Labels),
            family="font_body", position = position_fill(vjust = 0.5), size=4,
            color="white", lineheight = 0.8) +
  scale_x_continuous(labels = function(x) paste0(x * 100, "%"),
                     expand = c(0.025,0)) +
  scale_fill_manual(name = "Composición",
                    values = Colores,
                    labels = Titulos_leyenda) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="bottom",
        legend.justification = "center",
        legend.title = element_text(size=12.5, vjust=0.5, family="font_title", margin=margin(r=20)),
        legend.title.position = "left",
        legend.text = element_text(size=10, family="font_subtitle"),
        legend.key.spacing.x = unit(0.33, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_subtitle", size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_subtitle", size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=5)


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

dir <- paste0(str_sub(getwd(), 1, -34), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Crear datos
Data <- data.frame(
  Hijxs = rep(c("Mujeres con hijos/as", "Varones con hijos/as"), each = 2),
  Tipo = rep(c("Porcentaje", "Restante"), 2),
  Valor = c(50, 50, 80, 20)
) %>%
  mutate(
    Hijxs = factor(Hijxs, levels = c("Varones con hijos/as", "Mujeres con hijos/as")),
    Relleno = factor(
      ifelse(Tipo == "Restante", "Restante", as.character(Hijxs)),
      levels = c("Restante", "Mujeres con hijos/as", "Varones con hijos/as")
    )
  )

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c(
  "Mujeres con hijos/as" = "#6963aa",
  "Varones con hijos/as" = "#1e7b34",
  "Restante" = "#cbc2ce"
)

# Gráfico
grafico <- ggplot(Data, aes(y = Hijxs, x = Valor, fill = Relleno)) +
  geom_col(width = 0.6) +
  geom_text(
    data = subset(Data, Tipo == "Porcentaje"),
    aes(x = 102, label = paste0(Valor, "%")),
    hjust = 0,
    fontface = "bold",
    size=5,
    family="font_body"
  ) +
  scale_fill_manual(values = Colores) +
  scale_x_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = paste0(seq(0, 100, 20), "%"),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(text=element_text(family="font_body"),
        legend.position="none",,
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family="font_subtitle", size=17.5, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill="white", color=NA))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=2)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=2)


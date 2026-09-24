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

# Crear datos
Data <- data.frame(Organismo = c("SUD", "OVFG", "Juzgados"),
                   Cantidad = c(21219, 13905, 10488),
                   Texto = c("Denuncias totales SUD", "OVFG", "Juzgados de VFG"),
                   Descripcion = c("Se establecen parámetros y filtros estandarizados para calificar denuncias de VFG.",
                                   "Análisis específico y filtración por operadores jurídicos de las denuncias que configuran VFG.",
                                   "Causas nuevas y causas en trámite.")) %>%
  mutate(xmin = 0 - Cantidad/2,
         xmax = 0 + Cantidad/2) %>%
  arrange(Cantidad) %>%
  mutate(Level = row_number()) %>%
  mutate(ymin = Level - 0.4,
         ymax = Level + 0.4) %>%
  mutate(ymin2 = Level - 0.6,
         ymax2 = Level - 1 + 0.6) %>%
  mutate(ymin2 = ifelse(Organismo == "Juzgados", NA, ymin2),
         ymax2 = ifelse(Organismo == "Juzgados", NA, ymax2),
         xmin2 = case_when(Organismo == "SUD" ~ -6893.5,
                           Organismo == "OVFG" ~ -5212.5,
                           Organismo == "Juzgados" ~ NA),
         xmax2 = case_when(Organismo == "SUD" ~ 6893.5,
                           Organismo == "OVFG" ~ 5212.5,
                           Organismo == "Juzgados" ~ NA))

Trapezoides <- do.call(rbind, lapply(1:nrow(Data), function(i){
  fila <- Data[i, ]
  if(!is.na(fila$xmin2)){
    data.frame(
      Organismo = fila$Organismo,
      x = c(fila$xmin2, fila$xmax2, fila$xmax, fila$xmin),
      y = c(fila$ymin2, fila$ymin2, fila$ymin, fila$ymin)
    )
  }
}))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("SUD" = "#4c2158",
             "OVFG" = "#1e7b34",
             "Juzgados" = "#ec6230")

# Grafico
grafico <- ggplot(Data, aes(x=Cantidad, y=Level, fill=Organismo)) +
  geom_polygon(data = Trapezoides, aes(x = x, y = y, group = Organismo, fill=Organismo), alpha = 0.5) +
  geom_rect(aes(xmin = xmin, xmax=xmax, ymin = ymin, ymax=ymax)) +
  geom_text(aes(x=0, y=Level, label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg")),
            family="font_body", size=17.5, fontface="bold", color="white") +
  geom_text(aes(x=xmax, label=str_wrap(Texto, width=15), color=Organismo),
            family="font_title", fontface="bold", size=12, nudge_x = 1000,
            hjust=0, lineheight = 0.9, vjust=0.5) +
  geom_text(aes(x=xmin, label=str_wrap(Descripcion, width=35)),
            family="font_title", color="grey20", size=6, nudge_x=-6000) +
  theme_void() +
  scale_fill_manual(values = Colores, name="") +
  scale_color_manual(values = Colores, name="") +
  scale_x_continuous(limits = c(-20000, 17500)) +
  theme(text=element_text(family="font_body"),
        legend.position = "none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill="white", color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=8)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=8)
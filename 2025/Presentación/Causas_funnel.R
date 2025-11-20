# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(tidyr)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Crear datos
Data <- data.frame(Organismo = c("S.E. 911","SUD", "OVFG", "Juzgados"),
                   Cantidad = c(84440, 20400, 13787, 11065),
                   Tamaño = c(84440/1.5, 20400, 13787, 11065),
                   Texto = c("Requerimientos totales S.E. 911", "Denuncias totales SUD", "OVFG", "Juzgados de VFG")) %>%
  mutate(xmin = 0 - Tamaño/2,
         xmax = 0 + Tamaño/2) %>%
  arrange(Tamaño) %>%
  mutate(Level = row_number()) %>%
  mutate(ymin = Level - 0.4,
         ymax = Level + 0.4) %>%
  mutate(ymin2 = Level - 0.6,
         ymax2 = Level - 1 + 0.6) %>%
  mutate(ymin2 = ifelse(Organismo == "Juzgados", NA, ymin2),
         ymax2 = ifelse(Organismo == "Juzgados", NA, ymax2),
         xmin2 = case_when(Organismo == "S.E. 911" ~ -14073.33,
                           Organismo == "SUD" ~ -6893.5,
                           Organismo == "OVFG" ~ -5212.5,
                           Organismo == "Juzgados" ~ NA),
         xmax2 = case_when(Organismo == "S.E. 911" ~ 14073.33,
                           Organismo == "SUD" ~ 6893.5,
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
})) %>%
  mutate(x = ifelse(Organismo == "OVFG" & y == 1.4, c(-5532.5, 5532.5), x)) %>%
  mutate(x = ifelse(Organismo == "S.E. 911" & y == 3.4, c(-10200, 10200), x))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("S.E. 911" = "#f93e35",
             "SUD" = "#ff621d",
             "OVFG" = "#0f216d",
             "Juzgados" = "#852f8c")

# Grafico
grafico <- ggplot(Data, aes(x=Cantidad, y=Level, fill=Organismo)) +
  geom_polygon(data = Trapezoides, aes(x = x, y = y, group = Organismo, fill=Organismo), alpha = 0.5) +
  geom_rect(aes(xmin = xmin, xmax=xmax, ymin = ymin, ymax=ymax)) +
  geom_text(aes(x=0, y=Level, label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg")),
            family="font_sans", size=20, fontface="bold", color="white") +
  geom_text(aes(x=xmax, label=str_wrap(Texto, width=20), color=Organismo),
            family="font_sans", fontface="bold", size=20, nudge_x = 2000, hjust=0, lineheight=0.75) +
  theme_void() +
  scale_fill_manual(values = Colores, name="") +
  scale_color_manual(values = Colores, name="") +
  scale_x_continuous(limits = c(-29000, 45000)) +
  theme(text=element_text(family="font_sans"),
        legend.position = "none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=24, height=12)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=24, height=12)
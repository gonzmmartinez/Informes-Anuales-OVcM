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
library(waffle)

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
Data <- data.frame(Tipo = c("Asesoramientos", "Apertura de legajos"),
                   Cantidad = c(2342, 1160)) %>%
  mutate(Porcentaje = round(100 * Cantidad / sum(Cantidad), 0)) %>%
  mutate(Tipo = factor(Tipo, levels = c("Asesoramientos", "Apertura de legajos")))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Asesoramientos" = "#119ca0",
             "Apertura de legajos" = "#ec6230")

# Gráfico
grafico <- ggplot(Data, aes(fill = Tipo, values = Cantidad/10)) +
  geom_waffle(n_rows = 14, size = 0.33, colour = "white") +
  annotate(geom="text", x=0, y=8+0.2, size=12, color="#119ca0",
           family="font_body", fontface="bold", hjust=1, vjust=0,
           label=paste0(formatC(round(Data %>% filter(Tipo == "Asesoramientos") %>% pull(Porcentaje), 1), format="fg", big.mark=".", decimal.mark=","), "%")) +
  annotate(geom="text", x=0, y=8-0.2, size=8, color="#119ca0",
           family="font_body", fontface="bold", hjust=1, vjust=1,
           label=formatC(Data %>% filter(Tipo == "Asesoramientos") %>% pull(Cantidad), format="fg", big.mark=".", decimal.mark=",")) +
  annotate(geom="text", x=26, y=8+0.2, size=12, color="#ec6230",
           family="font_body", fontface="bold", hjust=0, vjust=0,
           label=paste0(formatC(round(Data %>% filter(Tipo == "Apertura de legajos") %>% pull(Porcentaje), 1), format="fg", big.mark=".", decimal.mark=","), "%")) +
  annotate(geom="text", x=26, y=8-0.2, size=8, color="#ec6230",
           family="font_body", fontface="bold", hjust=0, vjust=1,
           label=formatC(Data %>% filter(Tipo == "Apertura de legajos") %>% pull(Cantidad), format="fg", big.mark=".", decimal.mark=",")) +
  annotate(geom="text", x=13, y=16+0.3, size=8, color="black",
           family="font_body", fontface="bold", hjust=0.5, vjust=0,
           label="Total de asistencias") +
  annotate(geom="text", x=13, y=16, size=12, color="black",
           family="font_body", fontface="bold", hjust=0.5, vjust=1,
           label=formatC(sum(Data$Cantidad), format="fg", big.mark=".", decimal.mark=",")) +
  scale_fill_manual(name = "Tipo de asistencia", values = Colores) +
  scale_x_continuous(expand=c(0,0)) +
  coord_equal(clip="off") +
  theme_void() +
  theme(text=element_text(family="font_body"),
        legend.position="bottom",
        legend.justification = "center",
        legend.title = element_text(size=12, family="font_title", face="bold", margin=margin(r=15)),
        legend.text = element_text(size=12, family="font_subtitle"),
        legend.key.spacing.x = unit(0.5, "cm"),
        legend.margin = margin(b=10, t=-10),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t=5,0,0,0),
        plot.background = element_rect(fill="white", color=NA))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=9, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=9, height=5)

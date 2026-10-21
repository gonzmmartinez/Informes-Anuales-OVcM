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

dir <- paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

Data <- data.frame(Año = rep(c(2024, 2025, 2026), each=2),
                   Tipo = rep(c("No subir ni difundir contenidos",
                                "Suprimir contenidos"), 3),
                   Cantidad = c(16, 1, 52, 17, 51, 14)) %>%
  mutate(Año = case_when(Año == 2024 ~ "2024 (todo el año)",
                         Año == 2025 ~ "2025 (todo el año)",
                         Año == 2026 ~ "2026 (primer semestre)"))

# Paleta
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Colores
Colores <- c("No subir ni difundir contenidos" = "#c72a29",
             "Suprimir contenidos" = "#7c428a")

# Gráfico
grafico <- ggplot(Data, aes(x = Año, y = Cantidad, group = Tipo)) +
  geom_col(aes(fill = Tipo), position = position_dodge(width = 0.85), width = 0.85) +
  geom_text(aes(y = Cantidad + max(Data$Cantidad)*0.025, label = Cantidad),
            position = position_dodge(width = 0.85), family = "font_body", size = 10,
            color = "black", hjust = 0.5, vjust = 0) +
  scale_fill_manual(values = Colores) +
  scale_y_continuous(limits = c(0, 60), expand=c(0.025,0),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark = ",", format="fg")) +
  theme_light() +
  theme(
    text=element_text(family="font_body"),
    legend.position="top",
    legend.justification = "center",
    legend.title=element_blank(),
    legend.text=element_text(size=20, family="font_title"),
    legend.key.spacing.x = unit(0.5, "cm"),
    plot.title=element_blank(),
    plot.subtitle=element_blank(),
    plot.caption=element_blank(),
    panel.border = element_blank(),
    axis.text.x = element_text(family="font_subtitle", size=20, margin=margin(t=10)),
    axis.title.x=element_blank(),
    axis.line.x = element_line(color="darkgrey", linewidth=0.5),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin=unit(c(0.25,0.25,0.25,0.25), "cm"),
    panel.grid=element_blank(),
    panel.grid.major=element_line(linewidth=0.5, color="grey95"),
    panel.grid.major.x=element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=10, height=5)
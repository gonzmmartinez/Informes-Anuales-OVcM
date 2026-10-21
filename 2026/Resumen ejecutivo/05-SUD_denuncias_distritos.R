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

Data <- data.frame(Distrito = rep(c("Centro", "Norte - Tartagal", "Norte - Orán",
                                    "Sur - Metán", "Sur - Anta", "Centro - Sede Cafayate"), 2),
                   X_2025 = c(67.3, 11.7, 10.1, 5.5, 3.8, 1.4),
                   X_2026 = c(68.7, 11.5, 9.5, 5.3, 3.8, 1.3)) %>%
  pivot_longer(cols = c("X_2025", "X_2026"),
               names_to = "Año",
               values_to = "Porcentaje") %>%
    mutate(Año = ifelse(Año == "X_2025", "2025 (año completo)", "2026 (primer semestre)"),
           Label = paste0(formatC(Porcentaje, format="fg", decimal.mark=","), "%"),
           Distrito = factor(Distrito, levels = c("Centro", "Norte - Tartagal", "Norte - Orán",
                                                   "Sur - Metán", "Sur - Anta", "Centro - Sede Cafayate")))

# Paleta
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Colores
Colores <- c("2025 (año completo)" = "#119ca0",
             "2026 (primer semestre)" = "#4c2158")

# Gráfico
grafico <- ggplot(Data, aes(x = Porcentaje, y = Distrito, group = Año)) +
  geom_col(aes(fill = Año), position = position_dodge(width = 0.85), width = 0.85) +
  geom_text(aes(x = Porcentaje + max(Data$Porcentaje)*0.01, label = Label, group = Año),
            position = position_dodge(width = 0.85), family = "font_body", size = 4,
            color = "black", hjust = 0, vjust = 0.5) +
  scale_fill_manual(values=Colores) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits = c(0, 72.5), expand=c(0.025,0),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark = ",", format="fg")) +
  theme_light() +
  theme(
    text=element_text(family="font_body"),
    legend.position="top",
    legend.justification = "center",
    legend.title=element_blank(),
    legend.text=element_text(size=12, family="font_title"),
    legend.key.spacing.x = unit(0.5, "cm"),
    plot.title=element_blank(),
    plot.subtitle=element_blank(),
    plot.caption=element_blank(),
    panel.border = element_blank(),
    axis.text.y=element_text(family="font_subtitle", size=15, margin=margin(r=10)),
    axis.title.y=element_blank(),
    axis.line.y = element_line(color="darkgrey", linewidth=0.5),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
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
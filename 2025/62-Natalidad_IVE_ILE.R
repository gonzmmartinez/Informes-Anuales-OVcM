# Limpiar todo
rm(list = ls())

# Librerías
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(googlesheets4)
library(lubridate)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Plantilla
link_ive_ile <- "https://docs.google.com/spreadsheets/d/1nnUI0CDjmjEg9KYtft80CXVX9QX0ub4N3l9-ttiULV8/edit?usp=sharing"

# Cargar datos
Raw <- read_sheet(ss = link_ive_ile, sheet = "IVE/ILE_mes")

Data <- Raw %>%
  mutate(Cantidad = round(Cantidad, 0)) %>%
  mutate(Año_mes = paste0(str_sub(Año, 3,4), "-", formatC(Mes_ord, width = 2, flag = "0")))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Crear gráfico
grafico <- ggplot(Data, aes(x = Año_mes, y = Cantidad)) +
  geom_vline(xintercept=13, linewidth=0.5, linetype=2, color="grey25") +
  annotate(geom="text", x=13.5, y=450, label=str_wrap("Sanción de la Ley N° 27.610 de Acceso a la Interrupción Voluntaria del Embarazo", width=30),
           size=3, color="grey25", family="font_sans", fontface="italic", hjust=0, lineheight=1) +
  geom_line(aes(color = Tipo, group = Tipo), linewidth = 1.5) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark=",", format="fg")) +
  scale_x_discrete(labels = rep(str_to_title(month(ymd("2000-01-01") + months(0:11),
                                                   label = TRUE, abbr = TRUE, locale = "es_ES.UTF-8")), 5)) +
  annotate(geom="text", label = formatC(2020:2024, format="fg", big.mark=".", decimal.mark=","),
           x=seq(from=6.5, to=54.5, by=12), y=-85, family="font_sans", size=5, color="grey10") +
  annotate(geom="segment", x=seq(from=1, to=60, by=12), xend=seq(from=1, to=60, by=12),
           y=-70, yend=-110, linewidth=0.5, color="grey") +
  labs(x = "Mes-Año", y = "Cantidad") +
  scale_color_manual(values=c("#2b42a0", "#852f8c")) +
  theme_light() +
  coord_cartesian(ylim = c(-10, 550), xlim=c(0.5, 60.5), clip="off", expand=FALSE) +
  theme(text = element_text(size=20, family="font"),
      axis.text.x = element_text(size = 7.5, angle=90, family="font_sans", margin=margin(t=5,b=0,l=0,r=0),
                                 vjust=0.5),
      axis.text.y = element_text(size=10, family="font_sans"),
      axis.title.x = element_text(size=12, family="font_sans", margin=margin(t=35)),
      axis.title.y = element_text(size=12, family="font_sans"),
      legend.title = element_text(size=12, family="font_serif", margin=margin(b=10)),
      legend.text = element_text(size=12, family="font_sans"),
      legend.key.spacing.y = unit(0.5, "cm"),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_text(size=12, family="font_sans"),
      plot.caption.position = "plot",
      panel.grid = element_blank(),
      panel.grid.major = element_line(color="grey95", linewidth = 0.5),
      panel.spacing.x = unit(0, "line"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=5)
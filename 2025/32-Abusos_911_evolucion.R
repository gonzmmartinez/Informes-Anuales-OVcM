# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)
library(lubridate)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

# Diccionarios
Mes <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                            "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                  Mes_num = 1:12)

Data <- Raw %>%
  left_join(Mes, by="Mes") %>%
  filter(Año %in% c(2024,2025), Tipo == "Abuso sexual") %>%
  group_by(Año, Mes, Mes_num, Accion) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Accion = factor(Accion,
                         levels = c("Llamadas", "Intervenciones", "Intervenciones SAMEC"))) %>%
  ungroup %>%
  mutate(Orden = paste0(str_sub(Año, 3,4), "-", formatC(Mes_num, width=2, flag="0")))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Llamadas" = "#ff9d27",
             "Intervenciones" = "#5ec5d4",
             "Intervenciones SAMEC" = "#d3335e")

# Gráfico
grafico <- ggplot(Data, aes(x=Orden, y=Cantidad, group = Accion)) +
  geom_line(aes(color=Accion), linewidth=2) +
  geom_point(aes(color=Accion), size=3) +
  geom_text(aes(label=Cantidad, color=Accion), size=3, family="font_sans", hjust=0.5, show_guide=FALSE, nudge_y=5) +
  annotate(geom="text", y=-20, x=c(6.5, 15.5), label=formatC(c(2024, 2025), big.mark=".", decimal.mark=",", format="d"),
           size=8, color="black", family="font_sans") +
  annotate(geom="segment", x=c(1,13,18), xend=c(1,13,18), y=-15, yend=-27.5, color="grey", linewidth=0.25) +
  theme_light() +
  labs(x="Mes-Año", y="Cantidad") +
  scale_color_manual(name="Tipo de requerimiento", values = Colores) +
  scale_x_discrete(labels = c(str_to_title(month(1:12, label = TRUE, abbr = TRUE, locale = "es_ES")), 
                              str_to_title(month(1:6, label = TRUE, abbr = TRUE, locale = "es_ES")))) +
  coord_cartesian(ylim = c(-5, 150), xlim=c(0.75, 18.25), clip="off", expand=FALSE) +
  theme(text=element_text(family="font_sans"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(size=15, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20, margin=margin(t=40)),
        axis.title.y = element_text(size=20),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=7)
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

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

# Diccionarios
Mes <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                            "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                  Mes_num = 1:12)

Data <- Raw %>%
  left_join(Mes, by="Mes") %>%
  filter(Año %in% c(2024,2025, 2026), Tipo %in% c("Abuso sexual", "Abuso sexual (tentativa)"),
         Accion != "Llamadas SAMEC") %>%
  group_by(Año, Mes, Mes_num, Accion) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Accion = factor(Accion,
                         levels = c("Llamadas", "Intervenciones", "Intervenciones SAMEC"))) %>%
  ungroup %>%
  mutate(Orden = paste0(str_sub(Año, 3,4), "-", formatC(Mes_num, width=2, flag="0")))

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Llamadas" = "#119ca0",
             "Intervenciones" = "#c72a29",
             "Intervenciones SAMEC" = "#ec6230")

# Gráfico
grafico <- ggplot(Data, aes(x=Orden, y=Cantidad, group = Accion)) +
  geom_line(aes(color=Accion), linewidth=2) +
  geom_point(aes(color=Accion), size=3) +
  geom_text(aes(label=Cantidad, color=Accion), size=3, family="font_body", hjust=0.5, show.legend=FALSE, nudge_y=5) +
  annotate(geom="text", y=-20, x=c(6.5, 18.5, 27.5), label=2024:2026,
           size=7, color="black", family="font_title", fontface="bold") +
  annotate(geom="segment", x=c(1,13,25,30), xend=c(1,13,25,30), y=-15, yend=-27.5,
           color="darkgrey", linewidth=0.25) +
  theme_light() +
  labs(x="Mes-Año", y="Cantidad") +
  scale_color_manual(name="Tipo de requerimiento", values = Colores) +
  scale_x_discrete(labels = str_to_title(month(c(1:12,1:12,1:6), label = TRUE, abbr = TRUE, locale = "Spanish_Argentina.utf8"))) +
  coord_cartesian(ylim = c(-5, 150), xlim=c(0.75, 30.25), clip="off", expand=FALSE) +
  theme(text=element_text(family="font_body"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_title", face="bold"),
        legend.text = element_text(size=12, family="font_title"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(family="font_title", size=12, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_title", size=10, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=20, margin=margin(t=40)),
        axis.title.y = element_text(family="font_title", size=20),
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


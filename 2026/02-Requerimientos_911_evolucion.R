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

# Fuentes
library(sysfonts)
library(showtext)

dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")

font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))

showtext_auto()

# Años
Año_1 <- 2026
Año_2 <- 2025

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes") %>%
  mutate(Accion = ifelse(Accion %in% c("Llamadas SAMEC", "Intervenciones SAMEC"), "Intervenciones SAMEC", Accion))

# Diccionarios
Mes <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
                 Mes_num = 1:12,
                 Semestre_num = rep(c(1,2), each=6))

Data <- Raw %>%
  filter(Tipo %ni% c("Abuso sexual", "Abuso sexual (tentativa)")) %>%
  mutate(Año = as.character(Año)) %>%
  left_join(Mes, by="Mes") %>%
  group_by(Año,Semestre_num) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  rbind(c("2023", 2, NA)) %>%
  mutate(Cantidad = as.numeric(Cantidad),
         Semestre_año = paste0(str_sub(Año, 4,5), "-", Semestre_num)) %>%
  arrange(Semestre_año) %>%
  mutate(Label = ifelse(is.na(Cantidad), "", formatC(Cantidad, big.mark=".", decimal.mark=",", format="d"))) %>%
  mutate(Año = ifelse(Año %in% c("2023", "2026"), paste0(Año, "*"), Año))

Totales <- Data %>%
  mutate(Cantidad = ifelse(is.na(Cantidad), 0, Cantidad)) %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Posiciones
pos_anios <- c(seq(1.5, 11.5, by = 2), 13)
pos_lineas <- c(0.25, seq(2.5, 12.5, by = 2), 13.75)

# Gráfico
grafico <- ggplot(Data, aes(x=Semestre_año, y=Cantidad, group=1)) +
  geom_col(aes(fill=Cantidad)) +
  
  geom_text(aes(label=Label), family="font_body", size=5,
            fontface="bold", color="white", nudge_y=-7500, hjust=0.5) +
  
  # Totales anuales
  geom_point(
    data=Totales,
    aes(x=pos_anios, y=150000, size=Cantidad, color=Cantidad)
  ) +
  
  geom_text(
    data=Totales,
    aes(
      x=pos_anios,
      y=147500,
      label=formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg")
    ),
    size=5, color="white", family="font_body", fontface="bold"
  ) +
  
  geom_text(
    data=Totales,
    aes(x=pos_anios, y=154000, label=Año),
    size=4, color="white", family="font_title"
  ) +
  
  # Años debajo del gráfico
  annotate(
    geom="text",
    y=-25000,
    x=pos_anios,
    label=2020:2026,
    size=8, color="black", family="font_body"
  ) +
  
  # Líneas divisorias
  annotate(
    geom="segment",
    x=pos_lineas,
    xend=pos_lineas,
    y=-30000,
    yend=175000,
    color="grey",
    linewidth=0.25
  ) +
  
  labs(
    title="",
    x="Semestre/Año",
    y="Cantidad",
    caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente."
  ) +
  
  scale_x_discrete(
    labels = c(rep(c("1°", "2°"), 6), "1°")
  ) +
  
  scale_y_continuous(
    labels = function(z) formatC(
      z, big.mark=".", decimal.mark=",", format="d"
    ),
    breaks = seq(from=0, to=100000, by=25000)
  ) +
  
  scale_fill_gradient2(
    high="#4c2158",
    low="#C289D2",
    mid="#833A98",
    midpoint=mean(Data$Cantidad, na.rm=TRUE)
  ) +
  
  scale_color_gradient2(
    high="#4c2158",
    low="#C289D2",
    mid="#833A98",
    midpoint=mean(Totales$Cantidad, na.rm=TRUE)
  ) +
  
  scale_size_continuous(range=c(25, 40)) +
  
  theme_light() +
  
  coord_cartesian(
    ylim=c(-5000, 175000),
    xlim=c(0.25, 14.0),
    clip="off",
    expand=FALSE
  ) +
  
  theme(
    text=element_text(family="font_body"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=12, family="font_body"),
    plot.title=element_text(size=20, family="font_title", face="bold"),
    plot.subtitle=element_text(size=15, family="font_subtitle"),
    plot.caption=element_text(
      size=10, family="font_body", face="italic",
      margin=margin(t=10)
    ),
    axis.text.x=element_text(
      size=15, margin=margin(t=5,r=0,b=5,l=0)
    ),
    axis.text.y=element_text(
      size=10, margin=margin(t=0,r=5,b=0,l=5)
    ),
    axis.title.x=element_text(size=15, margin=margin(t=40)),
    axis.title.y=element_text(size=15),
    plot.margin=unit(c(0.5,0.5,0.5,0.5), "cm"),
    panel.grid=element_blank(),
    panel.grid.major=element_line(
      linewidth=0.5, color="grey95"
    ),
    panel.grid.major.x=element_blank()
  )

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=14, height=7)


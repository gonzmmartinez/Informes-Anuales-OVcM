# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(googlesheets4)
library(lubridate)

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

# Diccionarios
Mes_ord <- data.frame(Mes = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
                     "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
             Ord = 1:12)

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa")

Data <- Raw %>%
  filter(Año %in% c(2025, 2026), Tipo %ni% c("Penal")) %>%
  group_by(Año, Mes) %>%
  summarise(Cantidad = sum(Cantidad), .groups = "drop") %>%
  left_join(Mes_ord, by = "Mes") %>%
  arrange(Año, Ord) %>%
  mutate(Label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format = "fg"),
         Pos = row_number())

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Posiciones
pos_anios <- c(0.5, 12.5, 18.5)
pos_lineas <- c(0.25, seq(2.5, 12.5, by = 2), 13.75)

# Gráfico
grafico <- ggplot(Data, aes(x = Pos, y=Cantidad)) +
  geom_col(aes(fill=Cantidad), width=0.9) +
  geom_text(aes(label=Label), size=5, family="font_body",
            color="black", hjust=0.5, nudge_y=max(Data$Cantidad)*0.05) +
  scale_x_continuous(breaks = Data$Pos,
                     labels = str_to_title(month(c(1:12, 1:6), label = TRUE, abbr = TRUE, locale = "Spanish_Argentina.utf8"))) +
  scale_fill_gradient(low="#D1B0D9", high="#7c428a") +
  labs(x="", y="Cantidad") +
  annotate(geom="text", y=-600, x=c(6.5, 15.5), label=c(2025,2026),
           size=8, color="black", family="font_title", fontface="bold") +
  annotate(geom="segment", x=c(0.5,12.5,18.5), xend=c(0.5,12.5,18.5),
           y=-100, yend=-750, color="grey", linewidth=0.5) +
  theme_light() +
  coord_cartesian(ylim = c(-100, 4600), xlim=c(0.25, 18.75), clip="off", expand=FALSE) +
  scale_alpha_continuous(range=c(0.5, 1)) +
  theme(
    text=element_text(family="font_body"),
    legend.position="none",
    legend.title=element_blank(),
    legend.text=element_text(size=12, family="font_body"),
    plot.title=element_blank(),
    plot.subtitle=element_blank(),
    plot.caption=element_blank(),
    axis.text.x=element_text(family="font_subtitle", size=17.5, margin=margin(t=5,r=0,b=5,l=0)),
    axis.title.x=element_text(family="font_subtitle", size=15, margin=margin(t=30)),
    axis.text.y=element_blank(),
    axis.title.y=element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(linewidth=0.5, color="darkgrey"),
    axis.ticks.y = element_blank(),
    panel.border = element_blank(),
    plot.margin=unit(c(0.25,0.25,0.25,0.25), "cm"),
    panel.grid=element_blank(),
    panel.grid.major=element_line(linewidth=0.5, color="grey95"),
    panel.grid.major.x=element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=7)


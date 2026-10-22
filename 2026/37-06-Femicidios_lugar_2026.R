# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

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

# Cargar datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1_n2tTaEXNYTv7fGRLLXt65W49wvFmE3eDpxiBZqmMZk/edit?usp=sharing",
                  sheet = "REGISTRO")

Data <- Raw %>%
  filter(Año == 2026) %>%
  select(Lugar_del_hecho) %>%
  group_by(Lugar_del_hecho) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Lugar_del_hecho = factor(Lugar_del_hecho, levels=c("Vivienda", "Vía pública", "Sin dato")))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Vivienda" = "#119ca0",
             "Vía pública" = "#ec6230",
             "Sin dato" = "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Lugar_del_hecho, y=Cantidad, fill=Lugar_del_hecho)) +
  geom_col() +
  geom_text(aes(label=Cantidad), family="font_body", color="black",
            nudge_y=max(Data$Cantidad)*0.05, size=10, vjust=0) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font_body", color="white", nudge_y=-max(Data$Cantidad)*0.05,
            size=10, fontface="bold", vjust=1) +
  labs(x="Lugar del hecho", y="Cantidad") +
  scale_fill_manual(values=Colores,
                      labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(breaks=seq(0, 8, by=2), limits=c(0, max(Data$Cantidad)*1.1),
                     labels = function(z) round(z,0)) +
  theme_light() +
  theme(text=element_text(family="font_title"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_title"),
        plot.title = element_text(size=20, family="font_title", face="bold"),
        plot.subtitle = element_text(size=15, family="font_title"),
        plot.caption = element_text(size=12, family="font_title", face="italic"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        axis.text.x = element_text(size=25, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=5)
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
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Cargar datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1_n2tTaEXNYTv7fGRLLXt65W49wvFmE3eDpxiBZqmMZk/edit?usp=sharing",
                  sheet = "REGISTRO")

Hijes <- c("Sin hijas/os", "1 hija/o", "2 hijas/os", "3 hijas/os", "4 hijas/os", "5 hijas/os", "No informa")

Data <- Raw %>%
  filter(Año == 2024) %>%
  select(Hijos) %>%
  mutate(Hijos = factor(Hijos, levels=Hijes)) %>%
  group_by(Hijos) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gr?fico
grafico <- ggplot(Data, aes(x=Hijos, y=Cantidad, fill=Cantidad)) +
  geom_col() +
  geom_text(aes(label=Cantidad), family="font_sans", color="black", nudge_y=0.3, size=8) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font_sans", color="white", nudge_y=-0.3, size=10, fontface="bold") +
  labs(x="Cantidad de hijas/os", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_gradient2(low="#ffb18e", high="#ff621d", labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(breaks=1:6, limits=c(0, max(Data$Cantidad)+1), labels = function(z) round(z,0)) +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_sans"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        axis.text.x = element_text(size=20, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)
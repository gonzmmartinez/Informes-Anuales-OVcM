# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(tidyr)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Cargar datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1_n2tTaEXNYTv7fGRLLXt65W49wvFmE3eDpxiBZqmMZk/edit?usp=sharing",
                  sheet = "REGISTRO")

Edades <- c("Menos de 18 años", "18-29 años", "30-39 años", "40-49 años", "50 años o más")

Data <- Raw %>%
  filter(Año == 2024) %>%
  select(Rango_etario) %>%
  group_by(Rango_etario) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad))

Data <- as.data.frame(Edades) %>%
  left_join(Data, by = join_by(Edades == Rango_etario)) %>%
  mutate(Cantidad = replace_na(Cantidad, 0),
         Porcentaje = replace_na(Porcentaje, 0)) %>%
  mutate(Edades = factor(Edades, levels=Edades))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Edades, y=Cantidad, fill=Cantidad)) +
  geom_col() +
  geom_text(aes(label=Cantidad), family="font_sans", color="black", nudge_y=0.25, size=8) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font_sans", color="white", nudge_y=-0.25, size=10, fontface="bold") +
  labs(x="Rango etario", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_gradient(low="#95a1d0", high="#2b42a0",
                       labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(breaks=1:5, limits=c(0, max(Data$Cantidad)+1), labels = function(z) round(z,0)) +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="none",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_sans"),
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        axis.text.x = element_text(size=20, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)

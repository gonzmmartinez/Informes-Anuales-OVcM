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
font_add_google("Barlow", "font")
showtext_auto()

# Cargar datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1_n2tTaEXNYTv7fGRLLXt65W49wvFmE3eDpxiBZqmMZk/edit?usp=sharing",
                  sheet = "REGISTRO")

Data <- Raw %>%
  filter(Año == 2024) %>%
  select(Lugar_del_hecho) %>%
  group_by(Lugar_del_hecho) %>%
  summarise(Cantidad = n()) %>%
  ungroup() %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Lugar_del_hecho = factor(Lugar_del_hecho, levels=c("Vivienda", "Vía pública", "Sin dato")))

# Gráfico
grafico <- ggplot(Data, aes(x=Lugar_del_hecho, y=Cantidad, fill=Lugar_del_hecho)) +
  geom_col() +
  geom_text(aes(label=Cantidad), family="font", color="black", vjust=-1, size=8) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark = ","), "%")),
            family="font", color="white", vjust=2, size=10, fontface="bold") +
  geom_text(aes(label=Lugar_del_hecho), family="font", color="white", vjust=3.5, size=10) +
  labs(x="Lugar del hecho", y="Cantidad") +
  scale_alpha_continuous(range=c(0.6,1)) +
  scale_fill_manual(values = c("#f2904c", "#1daa6a", "#1da1aa"), labels = function(x) str_wrap(x, width = 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(breaks=1:3, limits=c(0, max(Data$Cantidad)+0.5), labels = function(z) round(z,0)) +
  theme_void() +
  theme(text=element_text(family="font"),
        legend.position="none",
        panel.grid = element_blank(),
        plot.background = element_rect(fill="white", colour=NA))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=7, height=7)
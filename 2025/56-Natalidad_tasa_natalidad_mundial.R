# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(eph)
library(janitor)
library(tidyr)
library(lubridate)
library(ggrepel)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Natalidad_global.csv")) %>%
  select(-c(Country.Code, Indicator.Name, Indicator.Code)) %>%
  filter(Country.Name %in% c("Latin America & Caribbean", "Middle East, North Africa, Afghanistan & Pakistan",
                             "Europe & Central Asia", "East Asia & Pacific", "South Asia",
                             "North America", "World", "Argentina")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Año",
               values_to = "Tasa") %>%
  mutate(Año = as.integer(sub("X", "", Año))) %>%
  filter(!is.na(Año)) %>%
  rename(Region = "Country.Name") %>%
  filter(Año >= 1995)

Data <- Raw %>%
  mutate(Region = case_when(Region == "Latin America & Caribbean" ~ "América latina y el Caribe",
                            Region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "Medio oriente y África del norte",
                            Region == "Sub-Saharan Africa" ~ "África subsahariana",
                            Region == "Europe & Central Asia" ~ "Europa y Asia central",
                            Region == "East Asia & Pacific" ~ "Asia del este y el Pacífico",
                            Region == "South Asia" ~ "Asia del sur",
                            Region == "North America" ~ "América del Norte",
                            Region == "World" ~ "Tasa mundial",
                            .default = Region)) %>%
  mutate(Grupo = case_when(Region == "América latina y el Caribe" ~ "América latina y el Caribe",
                           Region == "Tasa mundial" ~ "Tasa mundial",
                           Region == "Argentina" ~ "Argentina",
                           .default = "Resto de regiones")) %>%
  mutate(Grupo = factor(Grupo, levels=c("Argentina", "América latina y el Caribe", "Tasa mundial", "Resto de regiones"))) %>%
  mutate(Region = factor(Region, levels = c("África subsahariana", "Asia del sur", "Medio oriente y África del norte",
                                            "Asia del este y el Pacífico", "Europa y Asia central",
                                            "América del Norte", "Tasa mundial", "América latina y el Caribe",
                                            "Argentina"))) %>%
  arrange(Grupo, Region, Año) %>%
  filter(!is.na(Tasa))

Labels <- Data %>%
  filter(Año == 2023) %>%
  mutate(Label = case_when(Region == "Argentina" ~ paste0("<span>Tasa de Argentina: **",
                                                          formatC(round(Tasa,1), big.mark=".", decimal.mark=",", format="fg"),
                                                          "**</span>"),
                           Region == "América latina y el Caribe" ~ paste0("<span>Tasa de Latinoamérica: **",
                                                                           formatC(round(Tasa,1), big.mark=".", decimal.mark=",", format="fg"),
                                                                           "**</span>"),
                           Region == "Tasa mundial" ~ paste0("<span>Tasa mundial: **",
                                                             formatC(round(Tasa, 1), big.mark = ".", decimal.mark = ",", format="fg"),
                                                             "**</span>"),
                           .default = NA)) %>%
  na.omit()

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Argentina" = "#a782ec",
             "América latina y el Caribe" = "#ff9d27",
             "Tasa mundial" = "#d3335e",
             "Resto de regiones" = "#ffe2be")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Tasa)) +
  geom_line(aes(color=Grupo, group=Region, linewidth=Grupo), lineend = "round") +
  geom_point(data=Labels, aes(color=Grupo), size=3, show.legend = FALSE) +
  geom_richtext(data=Labels, aes(label=Label, color=Grupo), size=3, family="font_sans",
                hjust=0, nudge_x=0.1, fill=NA, label.colour = NA, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(from=1995, to=2025, by=5),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg")) +
  scale_y_continuous(labels = function(z) formatC(z, format = "fg", big.mark = ".", decimal.mark = ","),
                     expand = c(0,0), breaks = seq(from=0, to=35, by=5)) +
  scale_linewidth_manual(values=c(2, 1.5, 1.5, 1)) +
  scale_color_manual(values=Colores) +
  coord_cartesian(xlim=c(1996, 2028), ylim=c(-1,36), clip="off") +
  labs(y="Tasa bruta de natalidad\n(nacimientos por cada 1.000 habitantes)") +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(color="grey90", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_sans", size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_sans", size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font_sans", lineheight = 1),
        axis.title.y = element_text(size=15, family="font_sans"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)

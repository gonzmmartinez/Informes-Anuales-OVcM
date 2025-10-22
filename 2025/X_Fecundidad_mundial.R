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

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Fecundidad_global_mundo.csv")) %>%
  select(-c(Country.Code, Indicator.Name, Indicator.Code)) %>%
  filter(Country.Name %in% c("Latin America & Caribbean", "Middle East, North Africa, Afghanistan & Pakistan",
                             "Europe & Central Asia", "East Asia & Pacific", "South Asia",
                             "North America", "World", "Argentina", "Sub-Saharan Africa")) %>%
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

# Colores
Paleta <- c("#5fad56", "#f2c14e", "#f78154", "#4d9078", "#b4436c")
Paleta2 <- c("#474E93", "#7E5CAD", "#b4436c", "#72BAA9", "#D5E7B5")

Colores <- c("Argentina" = "#72BAA9",
             "América latina y el Caribe" = "#f78154",
             "Tasa mundial" = "#7E5CAD",
             "Resto de regiones" = "#D5E7B5")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Tasa)) +
  geom_line(aes(color=Grupo, group=Region, linewidth=Grupo), lineend = "round") +
  scale_x_continuous(breaks = seq(from=1995, to=2025, by=5),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg")) +
  scale_y_continuous(labels = function(z) formatC(z, format = "f", digits = 1, big.mark = ".", decimal.mark = ","),
                     expand = c(0,0), breaks = seq(from=0, to=6.5, by=1)) +
  scale_linewidth_manual(values=c(2, 1.5, 1.5, 1)) +
  scale_color_manual(values=Colores) +
  coord_cartesian(xlim=c(1995, 2025), ylim=c(-0.05,5), clip="off") +
  labs(y="Tasa global de fecundidad\n(promedio de hijos/as por mujer)") +
  theme_linedraw() +
  theme(text=element_text(family="font"),
        legend.position="bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid = element_line(color="grey90", linewidth = 0.5),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font", lineheight = 1),
        axis.title.y = element_text(size=15, family="font"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

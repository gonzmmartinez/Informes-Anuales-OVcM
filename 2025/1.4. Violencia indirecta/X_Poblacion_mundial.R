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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Poblacion_tasa_crecimiento.csv")) %>%
  select(-c(Country.Code, Indicator.Name, Indicator.Code)) %>%
  filter(Country.Name %in% c("Latin America & Caribbean", "Middle East, North Africa, Afghanistan & Pakistan",
                             "Sub-Saharan Africa", "Europe & Central Asia", "East Asia & Pacific", "South Asia",
                             "North America", "World")) %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Año",
               values_to = "Tasa") %>%
  mutate(Año = as.integer(sub("X", "", Año))) %>%
  filter(!is.na(Año)) %>%
  rename(Region = "Country.Name")

Data <- Raw %>%
  mutate(Region = case_when(Region == "Latin America & Caribbean" ~ "América latina y el Caribe",
                            Region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "Medio oriente y África del norte",
                            Region == "Sub-Saharan Africa" ~ "África subsahariana",
                            Region == "Europe & Central Asia" ~ "Europa y Asia central",
                            Region == "East Asia & Pacific" ~ "Asia del este y el Pacífico",
                            Region == "South Asia" ~ "Asia del sur",
                            Region == "North America" ~ "América del Norte",
                            Region == "World" ~ "Tasa mundial")) %>%
  mutate(Grupo = case_when(Region == "América latina y el Caribe" ~ "América latina y el Caribe",
                                  Region == "Tasa mundial" ~ "Tasa mundial",
                                  .default = "Resto de regiones")) %>%
  mutate(Grupo = factor(Grupo, levels=c("América latina y el Caribe", "Tasa mundial", "Resto de regiones"))) %>%
  mutate(Region = factor(Region, levels = c("África subsahariana", "Asia del sur", "Medio oriente y África del norte",
                                            "Asia del este y el Pacífico", "Europa y Asia central",
                                            "América del Norte", "Tasa mundial", "América latina y el Caribe"))) %>%
  arrange(Grupo, Region, Año)

# Colores
Paleta <- c("#5fad56", "#f2c14e", "#f78154", "#4d9078", "#b4436c")
Paleta2 <- c("#474E93", "#7E5CAD", "#b4436c", "#72BAA9", "#D5E7B5")

Colores <- c("América latina y el Caribe" = "#f78154",
             "Tasa mundial" = "#7E5CAD",
             "Resto de regiones" = "#D5E7B5")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Tasa)) +
  geom_line(aes(color=Grupo, group=Region, linewidth=Grupo), lineend = "round") +
  scale_x_continuous(breaks = seq(from=1960, to=2025, by=5),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg")) +
  scale_y_continuous(labels = function(z) formatC(z, format = "f", digits = 1, big.mark = ".", decimal.mark = ","),
                     expand = c(0,0), breaks = seq(from=0, to=4, by=0.5)) +
  scale_linewidth_manual(values=c(2, 2, 1)) +
  geom_text(data=Data %>% filter(Año == 2024), (aes(label=str_wrap(Region, width=15), color=Grupo, group=Region)),
                                                size=2.5, family="font", hjust=0, nudge_x=0.5, lineheight=0.75) +
  scale_color_manual(values=Colores) +
  coord_cartesian(xlim=c(1962, 2027), ylim=c(-0.05,4), clip="off") +
  labs(y="Tasa de crecimiento anual de la población") +
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
        panel.grid = element_line(color="grey95", linewidth = 0.5),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font"),
        axis.title.y = element_text(size=15, family="font"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)
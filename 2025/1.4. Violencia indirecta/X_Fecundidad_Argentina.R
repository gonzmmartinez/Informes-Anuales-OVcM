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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Nacidos_Vivos_Jurisdiccion.csv"))

Data <- Raw %>%
  mutate(indice_tiempo = as.numeric(str_sub(indice_tiempo, start=1, end=4))) %>%
  pivot_longer(cols=!indice_tiempo,
               names_to = "Provincia",
               values_to = "Cantidad") %>%
  rename(Año = "indice_tiempo") %>%
  mutate(Provincia = case_when(Provincia == "total_argentina" ~ "Total país",
                               Provincia == "capital_federal" ~ "Ciudad Autónoma de Buenos Aires",
                               Provincia == "buenos_aires" ~ "Buenos Aires",
                               Provincia == "catamarca" ~ "Catamarca",
                               Provincia == "cordoba" ~ "Córdoba",
                               Provincia == "corrientes" ~ "Corrientes",
                               Provincia == "chaco" ~ "Chaco",
                               Provincia == "chubut" ~ "Chubut",
                               Provincia == "entre_rios" ~ "Entre Ríos",
                               Provincia == "formosa" ~ "Formosa",
                               Provincia == "jujuy" ~ "Jujuy",
                               Provincia == "la_pampa" ~ "La Pampa",
                               Provincia == "la_rioja" ~ "La Rioja",
                               Provincia == "mendoza" ~ "Mendoza",
                               Provincia == "misiones" ~ "Misiones",
                               Provincia == "neuquen" ~ "Neuquén",
                               Provincia == "rio_negro" ~ "Río Negro",
                               Provincia == "salta" ~ "Salta",
                               Provincia == "san_juan" ~ "San Juan",
                               Provincia == "san_luis" ~ "San Luis",
                               Provincia == "santa_cruz" ~ "Santa Cruz",
                               Provincia == "santa_fe" ~ "Santa Fé",
                               Provincia == "santiago_del_estero" ~ "Santiago del Estero",
                               Provincia == "tucuman" ~ "Tucumán",
                               Provincia == "tierra_del_fuego" ~ "Tierra del Fuego")) %>%
  
  filter(Año >= 1980)

Data <- Raw %>%
  filter(Provincia %in% Provincias) %>%
  mutate(Grupo = case_when(Provincia == "Salta" ~ "Salta",
                           Provincia == "Argentina" ~ "Tasa nacional",
                           .default = "Resto de provincias")) %>%
  mutate(Grupo = factor(Grupo, levels=c("Salta", "Tasa nacional", "Resto de provincias"))) %>%
  mutate(Provincia = factor(Provincia, levels = c("Ciudad Autónoma de Buenos Aires", "Buenos Aires", "Catamarca",
                                                  "Chaco", "Chubut", "Córdoba", "Corrientes", "Entre Ríos", "Formosa", "Jujuy",
                                                  "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro",
                                                  "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
                                                  "Tierra del Fuego, Antártida e Islas del Atlántico Sur", "Tucumán",
                                                  "Argentina", "Salta"))) %>%
  arrange(Grupo, Provincia, Año)

# Colores
Paleta <- c("#5fad56", "#f2c14e", "#f78154", "#4d9078", "#b4436c")
Paleta2 <- c("#474E93", "#7E5CAD", "#b4436c", "#72BAA9", "#D5E7B5")

Colores <- c("Salta" = "#f78154",
             "Tasa nacional" = "#7E5CAD",
             "Resto de provincias" = "#D5E7B5")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=TGF)) +
  geom_line(aes(color=Grupo, group=Provincia, linewidth=Grupo), lineend = "round") +
  scale_x_continuous(breaks = seq(from=1960, to=2025, by=5),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg")) +
  scale_y_continuous(labels = function(z) formatC(z, format = "f", digits = 1, big.mark = ".", decimal.mark = ","),
                     expand = c(0,0)) +
  scale_linewidth_manual(values=c(2, 2, 1)) +
  geom_text(data=Data %>% filter(Año == 2022), (aes(label=str_wrap(Provincia, width=15), color=Grupo, group=Provincia)),
                                                size=2.5, family="font", hjust=0, nudge_x=0.5, lineheight=0.75) +
  scale_color_manual(values=Colores) +
  coord_cartesian(xlim=c(1960, 2024), ylim=c(0,6), clip="off") +
  labs(y="Tasa de crecimiento anual media de la población") +
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
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)
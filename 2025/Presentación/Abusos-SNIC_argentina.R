# Limpiar todo
rm(list = ls())

# Librerias
library(openxlsx)
library(tidyverse)
library(ggforce)
library(ggplot2)
library(ggthemes)
library(devtools)
library(rgdal)
library(geogrid)
library(sf)
library(tmap)
library(readxl)
library(ggpmisc)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Cargar datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/14m3Y-OdpPdvq0QLcFDZ7c0diTdoExJOHmOaY70qaeV8/edit?usp=sharing",
                  sheet = "Abuso_SNIC")

Data <- Raw %>%
  arrange(desc(Tasa_2024)) %>%
  top_n(10, Tasa_2024) %>%
  mutate(Variación = paste0(formatC(round(Variación*100, 1), big.mark=".", decimal.mark=",", digits=1, format="f"), "%"))

# Cargar shape
Mapa_Argentina <- rnaturalearth::ne_states(country = c("argentina"), returnclass = "sf") %>%
  mutate(name = ifelse(name == "Ciudad de Buenos Aires", "CABA", name))

# Modificar datos
Data_abusos <- Raw %>%
  rename(name = "Provincia")
Mapa_Argentina <- Mapa_Argentina %>%
  left_join(Data_abusos, by="name") %>%
  replace(is.na(.), 0)

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Tabla
Tabla <- Data %>%
  select(Provincia, Cant_2024, Tasa_2024) %>%
  mutate(Cant_2024 = formatC(Cant_2024, big.mark=".", decimal.mark=",", format="fg"),
         Tasa_2024 = formatC(round(Tasa_2024,1), big.mark=".", decimal.mark=",", format="fg")) %>%
  rename("Cant. 2024" = "Cant_2024", "Tasa 2024" = "Tasa_2024")

# Theme
my_ttheme <- 
  gridExtra::ttheme_default(base_colour="black", base_size=12, base_family="font",
                            colhead = list(fg_params=list(col="white"),
                                           bg_params=list(fill="#f93e35")),
                            core = list(bg_params=list(fill = "grey95", col = "white")))

# Grafico
grafico <- ggplot(Mapa_Argentina) +
  geom_sf(color="black", aes(fill=Tasa_2024)) +
  geom_sf_text(aes(label=formatC(round(Tasa_2024,1), big.mark=".", decimal.mark=",")),
               color="black", family="font_sans", size=3, show_guide=FALSE) +
  annotate(geom="table", x=-42, y=-54, label = list(Tabla), table.theme=my_ttheme) +
  theme_void() +
  guides(fill = guide_colorbar(theme = theme(legend.frame = element_rect(colour = "black")))) +
  scale_fill_gradient2(name=str_wrap("Tasa de delitos contra la integridad sexual por 100.000 habitantes (2024)", 30),
                       low="white", high="#f93e35", midpoint=diff(range(Raw$Tasa_2024))/2, limits=c(min(Raw$Tasa_2024),max(Raw$Tasa_2024)),
                       breaks=seq(20, 160, by=20)) +
  theme(text=element_text(family="font_sans"),
        plot.title = element_text(size=20, family="font_sans", face="bold"),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.margin = margin(t=0,r=0,b=0,l=0),
        legend.position = "left",
        legend.justification = "center",
        legend.margin = margin(t=50, r=-20, b=0, l=5),
        legend.title = element_text(size=10, family="font_sans", margin=margin(t=0,r=10,b=0,l=0), angle=90),
        legend.title.position = "left",
        legend.text = element_text(size=5, family="font_sans"),
        legend.ticks = element_line(color="black")) +
  guides(color = guide_legend( 
    override.aes=list(shape = 16)))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=8)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=8)
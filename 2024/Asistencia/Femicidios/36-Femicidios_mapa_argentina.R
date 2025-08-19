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

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Cargar datos
Raw <- read_excel(path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Femicidios 2014-2024 - modificado.xlsx"),
                  sheet="En filas")

Data <- Raw %>%
  filter(Año == 2023)

# Cargar shape
Mapa_Argentina <- rnaturalearth::ne_states(country = c("argentina"), returnclass = "sf") %>%
  mutate(name = ifelse(name == "Ciudad Autónoma de Buenos Aires", "CABA", name))

# Modificar datos
Data_femicidios <- Data %>%
  rename(name = "Provincia")
Mapa_Argentina <- Mapa_Argentina %>%
  left_join(Data_femicidios, by="name") %>%
  replace(is.na(.), 0)

# Colores
Colores <- colorRampPalette(c("white", "#191717"))(2)

# Tabla
Tabla <- Data_femicidios %>%
  select(name, Tasa) %>%
  rename(Provincia = "name") %>%
  arrange(desc(Tasa)) %>%
  mutate(Tasa = formatC(round(Tasa,2), big.mark=".", decimal.mark = ",")) %>%
  top_n(n=10)

# Theme
my_ttheme <- 
  gridExtra::ttheme_default(base_colour="black", base_size=8, base_family="font",
                            colhead = list(fg_params=list(col="white"),
                                           bg_params=list(fill="#1da1aa")),
                            core = list(bg_params=list(fill = "#f2f2f2", col = "white")))

# Grafico
grafico <- ggplot(Mapa_Argentina) +
  geom_sf(color="black", aes(fill=Tasa)) +
  geom_sf_text(aes(label=formatC(round(Tasa,2), big.mark=".", decimal.mark=",")),
               color="black", family="font", size=2, show_guide=FALSE) +
  annotate(geom="table", x=-45, y=-47, label = list(Tabla), table.theme=my_ttheme) +
  theme_void() +
  guides(fill = guide_colorbar(theme = theme(legend.frame = element_rect(colour = "black")))) +
  scale_fill_gradient2(name="Tasa de femicidios", low="white", high="#1da1aa", midpoint=0.7, limits=c(min(Data$Tasa),max(Data$Tasa)), breaks=seq(0.5,2, by=0.5)) +
  theme(text=element_text(family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.margin = margin(t=0,r=0,b=0,l=0),
        plot.background = element_rect(fill="white", color=NA),
        legend.position = "left",
        legend.justification = "center",
        legend.margin = margin(t=50, r=-20, b=0, l=5),
        legend.title = element_text(size=10, family="font", margin=margin(t=0,r=0,b=10,l=0)),
        legend.text = element_text(size=5, family="font"),
        legend.ticks = element_line(color="black")) +
  guides(color = guide_legend( 
    override.aes=list(shape = 16)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=8)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/pdf/"),
       plot=grafico, dpi=72, width=8, height=8)
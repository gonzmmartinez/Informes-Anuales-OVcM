# Limpiar todo
rm(list = ls())

# Librerias
library(openxlsx)
library(tidyverse)
library(ggforce)
library(ggplot2)
library(ggthemes)
library(devtools)
library(geogrid)
library(sf)
library(tmap)
library(readxl)
library(ggpmisc)
library(googlesheets4)

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
                  sheet = "Tasa_nacional")

Data <- Raw %>%
  filter(Año == 2025)

# Cargar shape
Mapa_Argentina <- rnaturalearth::ne_states(country = c("argentina"), returnclass = "sf") %>%
  mutate(name = ifelse(name == "Ciudad de Buenos Aires", "CABA", name))

# Modificar datos
Data_femicidios <- Data %>%
  rename(name = "Provincia")
Mapa_Argentina <- Mapa_Argentina %>%
  left_join(Data_femicidios, by="name") %>%
  replace(is.na(.), 0)

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Grafico
grafico <- ggplot(Mapa_Argentina) +
  geom_sf(color="black", aes(fill=Tasa)) +
  geom_sf_text(aes(label=formatC(round(Tasa,2), big.mark=".", decimal.mark=",")),
               color="black", family="font_body", size=3, show.legend=FALSE) +
  theme_void() +
  guides(fill = guide_colorbar(direction = "horizontal", barwidth = unit(4, "cm"),
                               theme = theme(legend.frame = element_rect(colour = "black")))) +
  scale_fill_gradient2(name="Tasa de femicidios", low="white", high="#ec6230",
                       midpoint=0.7, limits=c(min(Data$Tasa),max(Data$Tasa)), breaks=seq(0.5,2, by=0.5)) +
  theme(text=element_text(family="font_title"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.margin = margin(t=-25,r=0,b=-25,l=0),
        plot.background = element_rect(fill="white", color=NA),
        legend.position = "right",
        legend.justification = "center",
        legend.title = element_text(size=12.5, family="font_title", hjust=0.5, face="bold",
                                    margin=margin(t=0,r=0,b=10,l=0)),
        legend.title.position = "top",
        legend.box.just = "center",
        legend.box.margin = margin(l=-40, r=0),
        legend.text = element_text(size=5, family="font_body"),
        legend.ticks = element_line(color="black")) +
  guides(color = guide_legend( 
    override.aes=list(shape = 16)))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=8)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=6, height=8)

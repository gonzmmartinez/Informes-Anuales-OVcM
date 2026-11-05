# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(forecast)
library(tseries)
library(googlesheets4)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"),
         italic = file.path(dir, "CreatoDisplay-RegularItalic.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1AO8SmJ45quqCCvg9nZ0vZqRLvxZMfAqB7uzOtXmu9Ro/edit?usp=sharing",
                  sheet = "Medidas")

# Crear serie temporal
Data <- Raw %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Año = ifelse(Año == "2026", "2026*", as.character(Año)))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad)) +
  geom_col(aes(fill=Cantidad), width=0.9) +
  geom_text(aes(y=Cantidad, label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="d")),
            family="font_body", fontface="bold",color="white", size=10, vjust=1,
            nudge_y = -max(Data$Cantidad)*0.04) +
  labs(title="",
       x="Año", y="Cantidad",
       caption = "* La cantidad corresponde al primer semestre.") +
  scale_y_continuous(limits=c(0,max(Data$Cantidad)*1.1),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="d"),
                     expand = c(0.025, 0)) +
  scale_x_discrete(expand = c(0.2, 0)) +
  theme_light() +
  scale_fill_gradient(low="#DF6D5E", high="#c72a29") +
  theme(text=element_text(family="font_title"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=15, family="font_title", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size=20, family="font_title", margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, family="font_title", margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20, family="font_title"),
        axis.title.y = element_text(size=20, family="font_title"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)

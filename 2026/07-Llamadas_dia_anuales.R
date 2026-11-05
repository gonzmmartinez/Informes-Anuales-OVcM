# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- function(x, table) !(x %in% table)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
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

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Dia") %>%
  filter(Accion == "Llamadas")

Levels <- c("Domingo", "Sábado", "Viernes", "Jueves", "Miércoles", "Martes", "Lunes")

Data <- Raw %>%
  group_by(Año, Dia) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Dia = factor(Dia, levels=Levels)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup %>%
  mutate(Año = ifelse(Año %in% c("2023", "2026"), paste0(Año, "*"), Año))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x="", y=Dia, fill=Porcentaje)) +
  geom_tile() +
  facet_wrap(~Año, nrow=1) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark=".", decimal.mark=","),"%")),
            family="font_body", size=5, color="white", alpha=0.7) +
  labs(y="Día de la semana",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  scale_fill_gradient(low="#b8d6ac", high="#4c2158") +
  theme_light() +
  theme(text=element_text(family="font_body"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=10, family="font_subtitle", face="italic"),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family="font_subtitle", size=15, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family="font_subtitle", size=15, margin = margin(t=0,r=5,b=0,l=0)),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=20, color="black", family="font_title",
                                  face="bold", margin=margin(t=10, b=10)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)

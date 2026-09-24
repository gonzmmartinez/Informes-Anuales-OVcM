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
library(lubridate)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add("font_title", file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body", file.path(dir, "RobotoSlab-Regular.ttf"))
font_add("font_body_b", file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Hora") %>%
  filter(Accion == "Llamadas")

Data <- Raw %>%
  group_by(Año, Hora) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Hora = factor(Hora),
         Año = as.character(Año)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad/sum(Cantidad) * 100) %>%
  ungroup %>%
  mutate(Año = ifelse(Año %in% c("2023", "2026"), paste0(Año, "*"), Año))

Test <- Raw %>%
  group_by(Año, Hora) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Año = formatC(Año, big.mark=".", decimal.mark=",")) %>%
  ungroup %>%
  pivot_wider(
    names_from = Año,
    values_from = Cantidad
  )

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x="", y=Hora, fill=Porcentaje)) +
  geom_tile() +
  facet_wrap(~Año, nrow=1) +
  geom_text(aes(label=paste0(round(Porcentaje,1),"%")), family="font_body", size=4, color="white", alpha=0.7) +
  labs(y="Hora del día",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  scale_y_discrete(limits = rev) + 
  scale_fill_gradient(low="#b8d6ac", high="#4c2158") +
  theme_light() +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=10, family="font_subtitle", face="italic"),
        panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15, family="font_subtitle", margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=20, family="font_subtitle", margin = margin(t=0,r=5,b=0,l=0)),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=20, color="black", family="font_title", face="bold",
                                  margin=margin(t=10, b=10)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=10)
# Limpiar todo
rm(list = ls())

# Funciones
`%nin%` <- function(x, table) !(x %in% table)

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

font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))

showtext_auto()

# Cargar datos
Raw <- read.csv2(
  file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),
              "/Datos/snic-departamentos-anual.csv")
)

# Modificar datos
Data <- Raw %>%
  filter(provincia_id == 66,
         codigo_delito_snic_id %in% c("10", "11", "11_1", "11_2", "11_3", "11_4", "11_5"),
         anio >= 2014) %>%
  mutate(Tipo = case_when(
    codigo_delito_snic_id == "10" ~ "Violaciones",
    codigo_delito_snic_id %in% c("11", "11_1", "11_2", "11_3", "11_4", "11_5") ~ "Otros delitos")) %>%
  group_by(anio, Tipo) %>%
  summarise(Cantidad = sum(cantidad_victimas), .groups = "drop") %>%
  rename(Año = "anio")

# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Violaciones" = "#c72a29",
             "Otros delitos" = "#119ca0")

Titulos_leyenda <- c("Violaciones" = str_wrap("Abusos sexuales con acceso carnal (violaciones)", width = 30),
                     "Otros delitos" = str_wrap("Otros delitos contra la integridad sexual", width = 30))

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_col(position="stack", width=0.9) +
  geom_text(aes(label = formatC(Cantidad, big.mark = ".", decimal.mark=",", format="fg")),
            family="font_body", position = position_stack(vjust = 0.5), size=5, color="white") +
  geom_text(data = Data %>% group_by(Año) %>% summarise(Cantidad = sum(Cantidad)), inherit.aes=FALSE,
            aes(x=Año, y=Cantidad, label = formatC(Cantidad, big.mark = ".", decimal.mark=",", format="fg")),
            family="font_subtitle", size=6, fontface="bold", color="black", nudge_y=75) +
  labs(y="Cantidad de víctimas", x="Año") +
  scale_fill_manual(name = str_wrap("Tipo de delito", width=20),
                    values = Colores,
                    labels = Titulos_leyenda) +
  scale_y_continuous(labels = function(x) formatC(x, big.mark = ".", decimal.mark = ",", format="fg"),
                     limits= c(0, 2000)) +
  scale_x_continuous(expand = c(0.01, 0.01), breaks = 2014:2025) +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_title"),
        legend.text = element_text(size=12, family="font_body"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_line(color="grey95", linewidth=0.5),
        axis.text.x = element_text(family="font_subtitle", size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_subtitle", size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20, family="font_subtitle"),
        axis.title.y = element_text(size=20, family="font_subtitle"))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)


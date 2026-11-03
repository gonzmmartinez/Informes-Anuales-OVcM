# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(ggtext)
library(tidyr)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar datos
Raw <- read.csv(paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Relevamiento_municipios - Areas.csv"))

# Modificar datos
Data <- Raw %>%
  mutate(across(c(X2017, X2020, X2022, X2024, X2026), as.character)) %>%
  pivot_longer(cols = c(X2017, X2020, X2022, X2024, X2026),
               names_to = "Año",
               values_to = "Cantidad") %>%
  mutate(Año = as.factor(str_sub(Año, 2, 5))) %>%
  mutate(Cantidad = as.numeric(na_if(Cantidad, "-"))) %>%
  group_by(Año) %>%
  mutate(Porcentaje = Cantidad / sum(Cantidad, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(Label = ifelse(Porcentaje <= 0.03, NA, paste0(formatC(round(100 * Porcentaje,1), digits=1, big.mark=".", decimal.mark=",", format="f"), "%")),
         Dimensiones.comparadas = ifelse(Dimensiones.comparadas == "Creadas", "Con área", Dimensiones.comparadas),
         Dimensiones.comparadas = factor(Dimensiones.comparadas, levels=c("No responde", "En proceso de creación", "Sin área", "Con área")))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce",
            "#d6a62e", "#3f6f9f", "#b05c7a")

# Definir colores
Colores <- c("Con área" = "#1e7b34",
             "En proceso de creación" = "#6963aa",
             "Absorbida" = "#119ca0",
             "Sin área" = "#ec6230",
             "No responde" = "#cbc2ce")

# Gráfico1
grafico <- ggplot(Data, aes(x=Año, y=Porcentaje, fill=Dimensiones.comparadas)) +
  geom_col(position=position_stack()) +
  geom_text(aes(label=Label), position=position_stack(vjust = 0.5),
            family="font_body", color="black") +
  theme_void() +
  scale_fill_manual(values = Colores) +
  theme(text=element_text(family="font_title"),
        legend.position = "right",
        plot.title = element_text(family="font_title", size=25, face="bold", hjust=0.5),
        plot.subtitle = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=15, family="font_title"),
        legend.key.spacing.y = unit(0.25, "cm"),
        axis.text.x = element_text(family="font_title", size=15),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=7, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=7, height=3.5)

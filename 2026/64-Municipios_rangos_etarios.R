# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)

# Funciones
`%nin%` = Negate(`%in%`)

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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2026.csv"))

Tabla <- data.frame(
  Rango_etario = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                   "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                   "70-74", "75-79", "80-84", "85-89", "90-94", "Sin dato"),
  Rango_ord = 1:20)

# Modificar datos
Data_0 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  mutate(Edad = as.numeric(Edad)) %>%
  mutate(
    Rango_etario = cut(
      Edad,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95),
      labels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
        "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
        "65-69", "70-74", "75-79", "80-84", "85-89", "90-94"
      ),
      right = FALSE
    ),
    Rango_etario = as.character(Rango_etario),
    Rango_etario = ifelse(is.na(Edad), "Sin dato", Rango_etario),
    Rango_etario = factor(
      Rango_etario,
      levels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
        "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
        "65-69", "70-74", "75-79", "80-84", "85-89", "90-94",
        "Sin dato"
      )
    )
  ) %>%
  left_join(Tabla, by = "Rango_etario") %>%
  group_by(Género, Rango_etario, Rango_ord) %>%
  summarise(Cantidad = n()) %>%
  ungroup %>%
  mutate(Cantidad = ifelse(Género == "Varón", Cantidad, -1*Cantidad),
         Género =ifelse(Género == "Varón", "Varones", "Mujeres"))

SE <- Data_0 %>%
  filter(Rango_etario == "Sin dato") %>%
  mutate(Cantidad = abs(Cantidad))

Data <- Data_0 %>%
  filter(Rango_etario != "Sin dato")

Porcentajes <- Data_0 %>%
  mutate(Cantidad = abs(Cantidad)) %>%
  group_by(Género) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Label = paste0(formatC(round(Porcentaje, 1), format="fg", decimal.mark=",", big.mark="."), "%"))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Mujeres" = "#6963aa",
             "Varones" = "#1e7b34")

# Gráfico
grafico <- ggplot(Data, aes(x=Cantidad, y=reorder(Rango_etario, Rango_ord), fill=Género)) +
  geom_col(position = "stack") +
  geom_text(aes(label = abs(Cantidad),
                hjust = ifelse(Género == "Mujeres", 1, 0),
                nudge_x = ifelse(Género == "Mujeres", -max(abs(Data$Cantidad))*0.005, max(abs(Data$Cantidad))*0.005)),
            family="font_body", size=3) +
  annotate(geom = "text", x = min(Data$Cantidad), y = 1.5, lineheight=0.9,
           label = paste0("Mujeres sin\nedad especificada: ",
                          formatC(subset(SE, Género == "Mujeres")$Cantidad, big.mark = ".",decimal.mark = ",", format = "fg")),
           family = "font_title", size = 3, color = "#6963aa", vjust=0.5) +
  annotate(geom = "text", x = max(Data$Cantidad), y = 1.5, lineheight=0.9,
           label = paste0("Varones sin\nedad especificada: ",
                          formatC(subset(SE, Género == "Varones")$Cantidad, big.mark = ".",decimal.mark = ",", format = "fg")),
           family = "font_title", size = 3, color = "#1e7b34", vjust=0.5) +
  annotate(geom = "text", x = min(Data$Cantidad), y = 15, lineheight=0.9,
           label = subset(Porcentajes, Género == "Mujeres")$Label, fontface="bold",
           family = "font_title", size = 10, color = "#6963aa", vjust=0.5, hjust=0.5) +
  annotate(geom = "text", x = max(Data$Cantidad), y = 15, lineheight=0.9,
           label = subset(Porcentajes, Género == "Varones")$Label, fontface="bold",
           family = "font_title", size = 10, color = "#1e7b34", vjust=0.5, hjust=0.5) +
  labs(title="",
       y="Rango etario", x="Cantidad de casos asistidos") +
  theme_light() +
  scale_fill_manual(name = "Género",
                    values = Colores) +
  scale_x_continuous(expand = c(0.1, 0),
                     labels = function(z) abs(z)) +
  theme(text=element_text(family="font_title"),
        legend.position = "top",
        legend.justification = "right",
        legend.title = element_text(size=12, family="font_title", face="bold",
                                    margin = margin(r=15)),
        legend.text = element_text(size=12, family="font_title"),
        legend.key.spacing.x = unit(0.5, "cm"),
        legend.margin = margin(0,0,0,0),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major = element_line(colour = "grey95", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_title", size=10, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_title", size=10, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_title", size=12.5),
        axis.title.y = element_text(family="font_title", size=12.5))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=6)
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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Registro_Asistencias_Completo_2026.csv")) %>%
  rename(Originarias = "Perteneciente.a.comunidades.originarias")

# Modificar datos
Data <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  mutate(Originarias = case_when(Originarias == "Sí" ~ "Sí",
                                 Originarias == "Si" ~ "Sí",
                                 Originarias == "No" ~ "No",
                                 Originarias == "NO" ~ "No",
                                 Originarias == "Sin dato" ~ "Sin dato")) %>%
  filter(Originarias != "Sin dato") %>%
  mutate(Originarias = factor(Originarias, levels=c("Sí", "No"))) %>%
  group_by(Originarias) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje_label = paste0(formatC(round(Porcentaje,1), decimal.mark=",", format="fg"), "%"),
         Cantidad_label = formatC(Cantidad, big.mark=".", format="fg"))

Total <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Originarias %in% c("Sin dato", "")) %>%
  mutate(Originarias = "Sin dato") %>%
  group_by(Originarias) %>%
  summarise(Cantidad = n())

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Sí" = "#ec6230",
             "No" = "#cbc2ce")

# Gráfico
grafico <- ggplot(Data, aes(x = Porcentaje, y = factor(1),
                            fill = factor(Originarias, levels = c("No", "Sí")))) +
  annotate(geom="text", x=-2, y=1+0.02, label=subset(Data, Originarias=="Sí")$Porcentaje_label,
           hjust=1, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=102, y=1+0.02, label=subset(Data, Originarias=="No")$Porcentaje_label,
           hjust=0, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=-2, y=1-0.02, label=subset(Data, Originarias=="Sí")$Cantidad_label,
           hjust=1, size=5, vjust=1, family="font_body") +
  annotate(geom="text", x=102, y=1-0.02, label=subset(Data, Originarias=="No")$Cantidad_label,
           hjust=0, size=5, vjust=1, family="font_body") +
  geom_col(width = 0.5) +
  scale_x_continuous(expand = c(0.1,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(breaks = c("Sí", "No"),
                      values = Colores,
                      name=str_wrap("¿Es una persona perteneciente a comunidades originarias?", 30)) +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit="cm"),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(family="font_title", size=12, face="bold", margin=margin(r=10)),
        legend.text = element_text(family="font_title", size=12),
        legend.key.spacing.x = unit(0.25, "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=1.5)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=1.5)
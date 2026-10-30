# Limpiar todo
rm(list = ls())

# Librerías
library(tidyverse)
library(ggtext)
library(cowplot)

# Funciones
`%ni%` = Negate(`%in%`)

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
  rename(Orientacion = "Orientación.sexual",
         Originarias = "Perteneciente.a.comunidades.originarias",
         Discapacidad = "Persona.con.discapacidad") %>%
  mutate(Orientacion = case_when(Orientacion == "Heterosexual" ~ "Heterosexual",
                                 Orientacion %in% c("Sin dato", "") ~ "Sin dato",
                                 .default = "Otra"),
         Originarias = case_when(Originarias == "Sí" ~ "Sí",
                               Originarias == "Si" ~ "Sí",
                               Originarias == "No" ~ "No",
                               Originarias == "NO" ~ "No",
                               Originarias %in% c("Sin dato", "") ~ "Sin dato"),
         Discapacidad = case_when(Discapacidad %in% c("Sí", "Si", "si", "SI") ~ "Sí",
                                  Discapacidad %in% c("No", "NO", "no") ~ "No",
                                  Discapacidad %in% c("Sin dato", "SD", "") ~ "Sin dato"))

# Modificar datos
Data1 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Orientacion %ni% c("Sin dato", "")) %>%
  mutate(Orientacion = factor(Orientacion,
                              levels=c("Heterosexual", "Otra"))) %>%
  group_by(Orientacion) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje_label = paste0(formatC(round(Porcentaje,1), decimal.mark=",", format="fg"), "%"),
         Cantidad_label = formatC(Cantidad, big.mark=".", format="fg"))

Total1 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Orientacion %in% c("Sin dato", "")) %>%
  mutate(Orientacion = "Sin dato") %>%
  group_by(Orientacion) %>%
  summarise(Cantidad = n())

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores1 <- c("Heterosexual" = "#119ca0",
             "Otra" = "#cbc2ce")

# Gráfico
grafico1 <- ggplot(Data1, aes(x = Porcentaje, y = factor(1),
                            fill = factor(Orientacion, levels = c("Otra", "Heterosexual")))) +
  annotate(geom="text", x=-2, y=1+0.02, label=subset(Data1, Orientacion=="Heterosexual")$Porcentaje_label,
           hjust=1, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=102, y=1+0.02, label=subset(Data1, Orientacion=="Otra")$Porcentaje_label,
           hjust=0, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=-2, y=1-0.02, label=subset(Data1, Orientacion=="Heterosexual")$Cantidad_label,
           hjust=1, size=5, vjust=1, family="font_body") +
  annotate(geom="text", x=102, y=1-0.02, label=subset(Data1, Orientacion=="Otra")$Cantidad_label,
           hjust=0, size=5, vjust=1, family="font_body") +
  geom_col(width = 0.5) +
  labs(x=paste0("Sin dato: ", formatC(Total1$Cantidad, format="fg", big.mark="."))) +
  scale_x_continuous(expand = c(0.1,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(breaks = c("Heterosexual", "Otra"),
                    values = Colores1,
                    name=str_wrap("Orientación sexual", 30)) +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit="cm"),
        axis.title.x = element_text(family="font_title", size=12.5, color="darkgrey", margin=margin(t=5)),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(family="font_title", size=15, face="bold", margin=margin(r=10)),
        legend.text = element_text(family="font_title", size=15),
        legend.margin = margin(b=10),
        legend.key.spacing.x = unit(0.25, "cm"))

# Modificar datos
Data2 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Originarias != "Sin dato") %>%
  mutate(Originarias = factor(Originarias, levels=c("Sí", "No"))) %>%
  group_by(Originarias) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje_label = paste0(formatC(round(Porcentaje,1), decimal.mark=",", format="fg"), "%"),
         Cantidad_label = formatC(Cantidad, big.mark=".", format="fg"))

Total2 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Originarias %in% c("Sin dato", "")) %>%
  mutate(Originarias = "Sin dato") %>%
  group_by(Originarias) %>%
  summarise(Cantidad = n())

Colores2 <- c("Sí" = "#ec6230",
             "No" = "#cbc2ce")

# Gráfico
grafico2 <- ggplot(Data2, aes(x = Porcentaje, y = factor(1),
                            fill = factor(Originarias, levels = c("No", "Sí")))) +
  annotate(geom="text", x=-2, y=1+0.02, label=subset(Data2, Originarias=="Sí")$Porcentaje_label,
           hjust=1, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=102, y=1+0.02, label=subset(Data2, Originarias=="No")$Porcentaje_label,
           hjust=0, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=-2, y=1-0.02, label=subset(Data2, Originarias=="Sí")$Cantidad_label,
           hjust=1, size=5, vjust=1, family="font_body") +
  annotate(geom="text", x=102, y=1-0.02, label=subset(Data2, Originarias=="No")$Cantidad_label,
           hjust=0, size=5, vjust=1, family="font_body") +
  geom_col(width = 0.5) +
  labs(x=paste0("Sin dato: ", formatC(Total2$Cantidad, format="fg", big.mark="."))) +
  scale_x_continuous(expand = c(0.1,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(breaks = c("Sí", "No"),
                    values = Colores2,
                    name=str_wrap("¿Es una persona perteneciente a comunidades originarias?", 30)) +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit="cm"),
        axis.title.x = element_text(family="font_title", size=12.5, color="darkgrey", margin=margin(t=5)),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(family="font_title", size=15, face="bold", margin=margin(r=10)),
        legend.text = element_text(family="font_title", size=15),
        legend.margin = margin(b=10),
        legend.key.spacing.x = unit(0.25, "cm"))

# Modificar datos
Data3 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Discapacidad != "Sin dato") %>%
  mutate(Discapacidad = factor(Discapacidad, levels=c("Sí", "No"))) %>%
  group_by(Discapacidad) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje_label = paste0(formatC(round(Porcentaje,1), decimal.mark=",", format="fg"), "%"),
         Cantidad_label = formatC(Cantidad, big.mark=".", format="fg"))

Total3 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Discapacidad %in% c("Sin dato", "")) %>%
  mutate(Discapacidad = "Sin dato") %>%
  group_by(Discapacidad) %>%
  summarise(Cantidad = n())

Colores3 <- c("Sí" = "#7c428a",
             "No" = "#cbc2ce")

# Gráfico
grafico3 <- ggplot(Data3, aes(x = Porcentaje, y = factor(1),
                            fill = factor(Discapacidad, levels = c("No", "Sí")))) +
  annotate(geom="text", x=-2, y=1+0.02, label=subset(Data3, Discapacidad=="Sí")$Porcentaje_label,
           hjust=1, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=102, y=1+0.02, label=subset(Data3, Discapacidad=="No")$Porcentaje_label,
           hjust=0, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=-2, y=1-0.02, label=subset(Data3, Discapacidad=="Sí")$Cantidad_label,
           hjust=1, size=5, vjust=1, family="font_body") +
  annotate(geom="text", x=102, y=1-0.02, label=subset(Data3, Discapacidad=="No")$Cantidad_label,
           hjust=0, size=5, vjust=1, family="font_body") +
  geom_col(width = 0.5) +
  labs(x=paste0("Sin dato: ", Total3$Cantidad)) +
  scale_x_continuous(expand = c(0.1,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(breaks = c("Sí", "No"),
                    values = Colores3,
                    name=str_wrap("¿Es una persona con una discapacidad?", 20)) +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit="cm"),
        axis.title.x = element_text(family="font_title", size=12.5, color="darkgrey", margin=margin(t=5)),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(family="font_title", size=15, face="bold", margin=margin(r=10)),
        legend.text = element_text(family="font_title", size=15),
        legend.margin = margin(b=10),
        legend.key.spacing.x = unit(0.25, "cm"))

# Layout
grafico <- plot_grid(grafico1, grafico2, grafico3, ncol=1,
                     rel_widths = c(1,1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)
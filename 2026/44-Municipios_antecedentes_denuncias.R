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
  rename(Antecedentes = "Antecedentes.de.violencia.contra.parejas.o.ex.parejas",
         Denuncias = "Existencia.de.denuncias.en.relación.al.caso.asistido",
         Medidas = "Existencia.de.medidas.de.protección") %>%
  mutate(Antecedentes = case_when(Antecedentes %in% c("S", "Si", "si", "Sí", "SI") ~ "Sí",
                                  Antecedentes %in% c("No") ~ "No",
                                  Antecedentes %in% c("Sin dato", "Sin datos", "") ~ "Sin dato"),
         Denuncias = case_when(Denuncias %in% c("S", "Si", "si", "Sí", "SI") ~ "Sí",
                               Denuncias %in% c("No", "NO",
                                                "NO HAY DENUNCIAS A PESAR DE HABER RECIBIDO AGRESIONES FISICAS ELLA Y SU BEBE",
                                                "NO HAST AAHORA", "no") ~ "No",
                               Denuncias %in% c("Sin dato", "Sin datos", "") ~ "Sin dato"),
         Medidas = case_when(Medidas %in% c("S", "Si", "si", "Sí", "SI") ~ "Sí",
                             Medidas %in% c("No", "NO", "no") ~ "No",
                             Medidas %in% c("Sin dato", "Sin datos", "") ~ "Sin dato"))

# Modificar datos
Data1 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Antecedentes %ni% c("Sin dato", "")) %>%
  mutate(Antecedentes = factor(Antecedentes, levels=c("Sí", "No"))) %>%
  group_by(Antecedentes) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje_label = paste0(formatC(round(Porcentaje,1), decimal.mark=",", format="fg"), "%"),
         Cantidad_label = formatC(Cantidad, big.mark=".", format="fg"))

Total1 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Antecedentes %in% c("Sin dato", "")) %>%
  mutate(Antecedentes = "Sin dato") %>%
  group_by(Antecedentes) %>%
  summarise(Cantidad = n())

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores1 <- c("Sí" = "#6963aa",
              "No" = "#cbc2ce")

# Gráfico
grafico1 <- ggplot(Data1, aes(x = Porcentaje, y = factor(1),
                              fill = factor(Antecedentes, levels = c("No", "Sí")))) +
  annotate(geom="text", x=-2, y=1+0.02, label=subset(Data1, Antecedentes=="Sí")$Porcentaje_label,
           hjust=1, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=102, y=1+0.02, label=subset(Data1, Antecedentes=="No")$Porcentaje_label,
           hjust=0, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=-2, y=1-0.02, label=subset(Data1, Antecedentes=="Sí")$Cantidad_label,
           hjust=1, size=5, vjust=1, family="font_body") +
  annotate(geom="text", x=102, y=1-0.02, label=subset(Data1, Antecedentes=="No")$Cantidad_label,
           hjust=0, size=5, vjust=1, family="font_body") +
  geom_col(width = 0.5) +
  labs(x=paste0("Sin dato: ", formatC(Total1$Cantidad, format="fg", big.mark="."))) +
  scale_x_continuous(expand = c(0.1,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(breaks = c("Sí", "No"),
                    values = Colores1,
                    name=str_wrap("Antecedentes de violencia contra parejas o ex-parejas", 30)) +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit="cm"),
        axis.title.x = element_text(family="font_title", size=17, color="darkgrey", margin=margin(t=5)),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(family="font_title", size=15, face="bold", margin=margin(r=10)),
        legend.text = element_text(family="font_title", size=15),
        legend.margin = margin(b=5),
        legend.key.spacing.x = unit(0.25, "cm"))

# Modificar datos
Data2 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Denuncias != "Sin dato") %>%
  mutate(Denuncias = factor(Denuncias, levels=c("Sí", "No"))) %>%
  group_by(Denuncias) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje_label = paste0(formatC(round(Porcentaje,1), decimal.mark=",", format="fg"), "%"),
         Cantidad_label = formatC(Cantidad, big.mark=".", format="fg"))

Total2 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Denuncias %in% c("Sin dato", "")) %>%
  mutate(Denuncias = "Sin dato") %>%
  group_by(Denuncias) %>%
  summarise(Cantidad = n())

Colores2 <- c("Sí" = "#c72a29",
             "No" = "#cbc2ce")

# Gráfico
grafico2 <- ggplot(Data2, aes(x = Porcentaje, y = factor(1),
                            fill = factor(Denuncias, levels = c("No", "Sí")))) +
  annotate(geom="text", x=-2, y=1+0.02, label=subset(Data2, Denuncias=="Sí")$Porcentaje_label,
           hjust=1, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=102, y=1+0.02, label=subset(Data2, Denuncias=="No")$Porcentaje_label,
           hjust=0, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=-2, y=1-0.02, label=subset(Data2, Denuncias=="Sí")$Cantidad_label,
           hjust=1, size=5, vjust=1, family="font_body") +
  annotate(geom="text", x=102, y=1-0.02, label=subset(Data2, Denuncias=="No")$Cantidad_label,
           hjust=0, size=5, vjust=1, family="font_body") +
  geom_col(width = 0.5) +
  labs(x=paste0("Sin dato: ", formatC(Total2$Cantidad, format="fg", big.mark="."))) +
  scale_x_continuous(expand = c(0.1,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(breaks = c("Sí", "No"),
                    values = Colores2,
                    name=str_wrap("Existencia de denuncias en relación al caso asistido", 30)) +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit="cm"),
        axis.title.x = element_text(family="font_title", size=17, color="darkgrey", margin=margin(t=5)),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(family="font_title", size=15, face="bold", margin=margin(r=10)),
        legend.text = element_text(family="font_title", size=15),
        legend.margin = margin(b=5),
        legend.key.spacing.x = unit(0.25, "cm"))

# Modificar datos
Data3 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Medidas != "Sin dato") %>%
  mutate(Medidas = factor(Medidas, levels=c("Sí", "No"))) %>%
  group_by(Medidas) %>%
  summarise(Cantidad = n()) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Porcentaje_label = paste0(formatC(round(Porcentaje,1), decimal.mark=",", format="fg"), "%"),
         Cantidad_label = formatC(Cantidad, big.mark=".", format="fg"))

Total3 <- Raw %>%
  filter(Municipio %in% c("Capital", "Rosario de la Frontera",
                          "Cachi", "San Ramón de la Nueva Orán")) %>%
  filter(Medidas %in% c("Sin dato", "")) %>%
  mutate(Medidas = "Sin dato") %>%
  group_by(Medidas) %>%
  summarise(Cantidad = n())

Colores3 <- c("Sí" = "#1e7b34",
             "No" = "#cbc2ce")

# Gráfico
grafico3 <- ggplot(Data3, aes(x = Porcentaje, y = factor(1),
                            fill = factor(Medidas, levels = c("No", "Sí")))) +
  annotate(geom="text", x=-2, y=1+0.02, label=subset(Data3, Medidas=="Sí")$Porcentaje_label,
           hjust=1, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=102, y=1+0.02, label=subset(Data3, Medidas=="No")$Porcentaje_label,
           hjust=0, size=7.5, vjust=0, family="font_body", fontface="bold") +
  annotate(geom="text", x=-2, y=1-0.02, label=subset(Data3, Medidas=="Sí")$Cantidad_label,
           hjust=1, size=5, vjust=1, family="font_body") +
  annotate(geom="text", x=102, y=1-0.02, label=subset(Data3, Medidas=="No")$Cantidad_label,
           hjust=0, size=5, vjust=1, family="font_body") +
  geom_col(width = 0.5) +
  labs(x=paste0("Sin dato: ", formatC(Total3$Cantidad, format="fg", big.mark="."))) +
  scale_x_continuous(expand = c(0.1,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_manual(breaks = c("Sí", "No"),
                    values = Colores3,
                    name=str_wrap("Existencia de medidas de protección", 30)) +
  theme_void() +
  theme(plot.background = element_rect(fill="white", color=NA),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit="cm"),
        axis.title.x = element_text(family="font_title", size=17, color="darkgrey", margin=margin(t=5)),
        legend.position = "top",
        legend.justification = "center",
        legend.title = element_text(family="font_title", size=15, face="bold", margin=margin(r=10)),
        legend.text = element_text(family="font_title", size=15),
        legend.margin = margin(b=5),
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
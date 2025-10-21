# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
library(eph)
library(janitor)
library(tidyr)
library(lubridate)
library(ggrepel)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Datos_EPH.csv")) %>%
  filter(ANO4 == 2025)

# Filtrar datos
Data0 <- Raw %>%
  filter(AGLOMERADO == 23, CH06 >= 14, ESTADO == 1) %>%
  organize_labels(type = "individual") 

Data0 <- organize_cno(Data0) %>%
  filter(JERARQUIA != "Ns.Nc")

# Calcular tabulados
Data <- calculate_tabulates(
  base = Data0,
  x = "JERARQUIA",
  y = "CH04",
  weights = "PONDERA") %>%
  rename(Jerarquia = "JERARQUIA/CH04") %>%
  pivot_longer(cols = c("Varon", "Mujer"),
               names_to = "Género",
               values_to = "Cantidad") %>%
  mutate(Género = paste0(Género, "es")) %>%
  mutate(Jerarquia = case_when(Jerarquia == "Jefes" ~ "Jefes/as",
                               Jerarquia == "Dirección" ~ "Dirección",
                               Jerarquia == "Trabajadores asalariados" ~ "Trabajadores/as asalariados/as",
                               Jerarquia == "Cuenta propia" ~ "Cuenta propia")) %>%
  group_by(Jerarquia) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Jerarquia = factor(Jerarquia, levels=c("Jefes/as", "Dirección", "Trabajadores/as asalariados/as", "Cuenta propia"))) %>%
  mutate(ymid = case_when(Jerarquia == "Cuenta propia" ~ 1,
                          Jerarquia == "Trabajadores/as asalariados/as" ~ 2,
                          Jerarquia == "Dirección" ~ 3,
                          Jerarquia == "Jefes/as" ~ 4)) %>%
  mutate(ymin = ymid - 0.3,
         ymax = ymid + 0.3) %>%
  mutate(xmin = case_when(Jerarquia == "Cuenta propia" & Género == "Varones" ~ 0,
                          Jerarquia == "Trabajadores/as asalariados/as" & Género == "Varones" ~ 50 - 37.5,
                          Jerarquia == "Dirección" & Género == "Varones" ~ 50 - 25,
                          Jerarquia == "Jefes/as" & Género == "Varones" ~ 50 - 12.5,
                          Jerarquia == "Cuenta propia" & Género == "Mujeres" ~ 100 - (Porcentaje * 1),
                          Jerarquia == "Trabajadores/as asalariados/as" & Género == "Mujeres" ~ 12.5 + (100 - Porcentaje) * 0.75,
                          Jerarquia == "Dirección" & Género == "Mujeres" ~ 25 + (100 - Porcentaje) * 0.5,
                          Jerarquia == "Jefes/as" & Género == "Mujeres" ~ 37.5 + (100 - Porcentaje) * 0.25)) %>%
  mutate(xmax = case_when(Jerarquia == "Cuenta propia" & Género == "Varones" ~ xmin + (Porcentaje * 1),
                          Jerarquia == "Trabajadores/as asalariados/as" & Género == "Varones" ~ xmin + (Porcentaje * 0.75),
                          Jerarquia == "Dirección" & Género == "Varones" ~ xmin + (Porcentaje * 0.5),
                          Jerarquia == "Jefes/as" & Género == "Varones" ~ xmin + (Porcentaje * 0.25),
                          Jerarquia == "Cuenta propia" & Género == "Mujeres" ~ 100,
                          Jerarquia == "Trabajadores/as asalariados/as" & Género == "Mujeres" ~ 100 - 12.5,
                          Jerarquia == "Dirección" & Género == "Mujeres" ~ 100 - 25,
                          Jerarquia == "Jefes/as" & Género == "Mujeres" ~ 100 - 37.5)) %>%
  mutate(Label_pos = ifelse(Género == "Mujeres", xmax+5, xmin-5))

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Mujeres" = "#ff621d",
             "Varones" = "#852f8c")

# Gr?fico
grafico <- ggplot(Data, aes(y=Jerarquia, x=Porcentaje)) +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=Género)) +
  geom_text(aes(x=Label_pos, label=paste0(formatC(round(abs(Porcentaje),1), big.mark = ".", decimal.mark = ","), "%")),
                size=5, color="black", family="font_sans", fontface="bold") +
  geom_vline(xintercept=50, linewidth=0.5, linetype=2, color="black") +
  scale_y_discrete(limits=rev, labels = function(z) str_wrap(z, width=15)) +
  scale_fill_manual(values = Colores) +
  scale_x_continuous(breaks = 50, labels="50%") +
  coord_cartesian(ylim = c(0.5, 4.5), xlim=c(-10, 110), clip="off", expand=FALSE) +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(0.5, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color="grey95"),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=17, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15, family="font_sans"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)
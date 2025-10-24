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
  filter(ANO4 == 2025, TRIMESTRE == 1)

# Filtrar datos
Data0 <- Raw %>%
  filter(AGLOMERADO == 23, CH06 >= 14, ESTADO == 1) %>%
  mutate(P21 = as.numeric(P21)) %>%
  filter(P21 != "0") %>%
  mutate(CH04 = as.factor(CH04))

Data <- organize_cno(Data0) %>%
  filter(JERARQUIA != "Ns.Nc") %>%
  mutate(Género = ifelse(CH04 == "1", "Varones", "Mujeres")) %>%
  rename(Jerarquia = "JERARQUIA") %>%
  group_by(Género, Jerarquia) %>%
  summarise(Mean = mean(P21),
            Median = median(P21)) %>%
  pivot_longer(cols = c("Mean", "Median"),
               names_to = "Medida",
               values_to = "Valor") %>%
  filter(Medida == "Mean") %>%
  mutate(Género = factor(Género, levels=c("Varones", "Mujeres"))) %>%
  mutate(Jerarquia = case_when(Jerarquia == "Trabajadores asalariados" ~ "Trabajadores/as asalariados/as",
                               Jerarquia == "Jefes" ~ "Jefes/as",
                               .default = Jerarquia)) %>%
  mutate(Jerarquia = factor(Jerarquia, levels=c("Cuenta propia", "Trabajadores/as asalariados/as", "Dirección", "Jefes/as")))

Lineas <- Data %>%
  select(-Medida) %>%
  pivot_wider(names_from = "Género",
              values_from = "Valor") %>%
  rename(ymin = "Mujeres",
         ymax = "Varones") %>%
  mutate(ymid = ymin + (ymax - ymin)/2,
         Label = paste0(formatC(round(100 * (ymax - ymin)/ymax, 1), format="fg", decimal.mark = ",", big.mark = "."), "%")) %>%
  arrange(Jerarquia) %>%
  mutate(x = 1:4 + 0.15)

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Colores
Colores <- c("Varones" = "#852f8c",
             "Mujeres" = "#ff621d")

# Gráfico
grafico <- ggplot(Data, aes(x=Jerarquia, y=Valor)) +
  geom_segment(data=Lineas, aes(x=Jerarquia, xend=Jerarquia, y=ymin, yend=ymax), color="lightgrey", linewidth=1.5) +
  geom_point(aes(color=Género), size=8) +
  geom_text(data=Lineas, aes(x=x, y=ymid, label=Label),
            family="font_sans", fontface="bold", size=10, color="black", hjust=0) +
  scale_color_manual(values = Colores) +
  scale_x_discrete(labels = function(z) str_wrap(z, width=20)) +
  scale_y_continuous(labels=function(z) paste0("$", formatC(z, big.mark = ".", decimal.mark=",", format="d")),
                     expand = c(0,0), breaks=seq(0,2000000, by=500000)) +
  coord_cartesian(xlim=c(1.25, 4.25), ylim=c(0,1750000), clip="off") +
  labs(y="Ingreso promedio") +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(0.5, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color="grey95"),
        axis.text.x = element_text(size=12, family="font_sans", margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, family="font_sans", margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font_sans"),
        axis.title.y = element_text(size=15, family="font_sans"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)
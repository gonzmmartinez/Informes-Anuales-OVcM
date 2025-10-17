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
library(ggh4x)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Datos_EPH.csv"))

Data <- Raw %>%
  mutate(ANO4 = as.character(ANO4)) %>%
  filter(AGLOMERADO == 23, CH06 >= 14, ANO4 == 2025) %>%
  organize_labels(type = "individual") %>%
  group_by(ANO4, CH04) %>%
  summarise(Poblacion = sum(PONDERA),
            Ocupados = sum(PONDERA[ESTADO == 1]),
            Desocupados = sum(PONDERA[ESTADO == 2]),
            PEA = Ocupados + Desocupados,
            Subocupados = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]) + sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Tasa_Actividad = round(PEA/Poblacion * 100, 1),
            Tasa_Empleo = round(Ocupados/Poblacion * 100, 1),
            Tasa_Desocupacion = round(Desocupados/PEA * 100, 1),
            Tasa_Subocupacion = round(Subocupados/PEA * 100, 1)) %>%
  ungroup %>%
  mutate(CH04 = ifelse(CH04 == "1", "Varones", "Mujeres")) %>%
  rename(Año = "ANO4", Género = "CH04") %>%
  select(Año, Género, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion, Tasa_Subocupacion) %>%
  pivot_longer(cols = starts_with("Tasa_"),
               names_to = "Indicador",
               values_to = "Valor") %>%
  mutate(Indicador = case_when(Indicador == "Tasa_Actividad" ~ "Actividad",
                               Indicador == "Tasa_Empleo" ~ "Empleo",
                               Indicador == "Tasa_Desocupacion" ~ "Desocupación",
                               Indicador == "Tasa_Subocupacion" ~ "Subocupación")) %>%
  mutate(Género = factor(Género, levels = c("Mujeres", "Varones"))) %>%
  mutate(Indicador = factor(Indicador, levels=c("Actividad", "Empleo", "Subocupación", "Desocupación")))

Brechas <- Data %>%
  pivot_wider(names_from="Género",
              values_from = "Valor") %>%
  select(-Año) %>%
  mutate(Brecha = abs(Varones - Mujeres)) %>%
  rowwise() %>%
  mutate(ypos = min(Varones, Mujeres))

# Colores
Colores <- c("Varones" = "#f2904c",
             "Mujeres" = "#1daa6a")

# Escalas
custom_y <- list(
  scale_y_continuous(limits = c(-5,80), breaks = seq(from=0, to=80, by=20),
                     labels = function(z) paste0(formatC(z, big.mark = ".", decimal.mark=",", format="fg"), "%")),
  scale_y_continuous(limits = c(-5,80), breaks = seq(from=0, to=80, by=20),
                     labels = function(z) paste0(formatC(z, big.mark = ".", decimal.mark=",", format="fg"), "%")),
  scale_y_continuous(limits = c(-1,20), breaks = seq(from=0, to=20, by=5),
                     labels = function(z) paste0(formatC(z, big.mark = ".", decimal.mark=",", format="fg"), "%")),
  scale_y_continuous(limits = c(-1,20), breaks = seq(from=0, to=20, by=5),
                     labels = function(z) paste0(formatC(z, big.mark = ".", decimal.mark=",", format="fg"), "%"))
)

# Gráfico
grafico <- ggplot(Data, aes(x=Género, y=Valor)) +
  geom_col(aes(x=Género, y=Valor, fill=Género)) +
  geom_text(aes(label=paste0(formatC(Valor, format = "f", digits = 1, decimal.mark = ","), "%")),
            family="font_sans", size=7.5, fontface="bold", nudge_y=c(-5,-5,-1.25,-1.25), color="white") +
  geom_text(data=Brechas, inherit.aes=FALSE,
            aes(x=c(1,1,2,2), y=ypos, label=paste0(formatC(Brecha, format = "f", digits = 1, decimal.mark = ","), "%")),
            family="font_sans", size=5, fontface="bold", nudge_y=c(6,6,1,1), color="black") +
  geom_text(data=Brechas, inherit.aes=FALSE,
            aes(x=c(1,1,2,2), y=ypos, label="Brecha:"),
            family="font_sans", size=2.5, fontface="bold", nudge_y=c(10,10,2,2), color="black") +
  labs(title="",
       x="Género", y="Valor de la tasa") +
  facet_wrap(~Indicador, nrow=1, ncol=4, dir="v", scales="free") +
  theme_light() +
  coord_cartesian(xlim=c(0.3, 2.7), clip="off", expand=FALSE) +
  facetted_pos_scales(y = custom_y) +
  scale_y_continuous(labels = function(z) paste0(formatC(z, big.mark = ".", decimal.mark=",", format="fg"), "%")) +
  scale_x_discrete(labels = function(z) paste0(str_sub(z, 6, -1), "°")) +
  scale_fill_manual(name="Género", values=Colores) +
  theme(text=element_text(family="font"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=10, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0, 0.5, 0.5, 0.5), "cm"),
        panel.spacing.y = unit(1, "cm"),
        strip.background = element_rect(color=NA, fill="#FE6244"),
        strip.text = element_text(size=15, color="white", family="font", face="bold"),
        legend.position= "top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=15),
        legend.box.margin=margin(5,5,5,5),
        legend.key.spacing.x = unit(0.5, "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=5)

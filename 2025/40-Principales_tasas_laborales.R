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
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Datos_EPH.csv"))

Data <- Raw %>%
  mutate(ANO4 = as.character(ANO4), TRIMESTRE = as.character(TRIMESTRE)) %>%
  filter(AGLOMERADO == 23, CH06 >= 14) %>%
  organize_labels(type = "individual") %>%
  group_by(ANO4, TRIMESTRE, CH04) %>%
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
  mutate(CH04 = ifelse(CH04 == "1", "Varones", "Mujeres"),
         TRIMESTRE = str_sub(TRIMESTRE, 1,1)) %>%
  rename(Año = "ANO4", Trimestre = "TRIMESTRE", Género = "CH04") %>%
  select(Año, Trimestre, Género, Tasa_Actividad, Tasa_Empleo, Tasa_Desocupacion, Tasa_Subocupacion) %>%
  pivot_longer(cols = starts_with("Tasa_"),
               names_to = "Indicador",
               values_to = "Valor") %>%
  mutate(Indicador = case_when(Indicador == "Tasa_Actividad" ~ "Actividad",
                               Indicador == "Tasa_Empleo" ~ "Empleo",
                               Indicador == "Tasa_Desocupacion" ~ "Desocupación",
                               Indicador == "Tasa_Subocupacion" ~ "Subocupación")) %>%
  mutate(Trimestre_Año = paste0(Año, "-", Trimestre)) %>%
  mutate(Género = factor(Género, levels = c("Mujeres", "Varones")))


# Crear un data.frame con las etiquetas por facet
indics <- if (is.factor(Data$Indicador)) levels(Data$Indicador) else unique(Data$Indicador)

df_labels <- do.call(rbind, lapply(indics, function(ind) {
  data.frame(
    Indicador = ind,
    x = c(2.5, 5.5),
    y = -27.5,
    label = formatC(c(2024, 2025), big.mark=".", decimal.mark=",", format="d"),
    stringsAsFactors = FALSE
  )
}))
df_labels$Indicador <- factor(df_labels$Indicador, levels = indics)

df_segments <- do.call(rbind, lapply(indics, function(ind) {
  data.frame(
    Indicador = ind,
    x = c(1, 5),
    xend = c(1, 5),
    y = -22.5,
    yend = -32.5,
    stringsAsFactors = FALSE
  )
}))
df_segments$Indicador <- factor(df_segments$Indicador, levels = indics)

# Colores
Colores <- c("Varones" = "#f2904c",
             "Mujeres" = "#1daa6a")

Labels <- Data %>%
  filter(Año == 2025, Trimestre == 1) %>%
  ungroup %>%
  mutate(xpos = 5.4) %>%
  mutate(Género = factor(Género, levels = c("Mujeres", "Varones")))

# Gráfico
grafico <- ggplot(Data, aes(x=Trimestre_Año, y=Valor)) +
  geom_line(aes(color=Género, group=Género), linewidth=2.5, lineend = 'round') +
  geom_point(data=Labels, aes(x=xpos-0.4, y=Valor, color=Género), size=3, show.legend = FALSE) +
  geom_label_repel(data=Labels, aes(x=xpos, y=Valor,
                                    label=paste0(formatC(Valor, format = "f", digits = 1, big.mark = ".", decimal.mark = ","), "%"),
                                    color=Género),
                   fill="white", family="font", box.padding=0.01, point.padding=0.01, force=0.001,
                   max.overlaps=Inf, min.segment.length=0, direction="y", vjust = 0.5, hjust=0.5, alpha=0.75,
                   show.legend = FALSE) +
  labs(title="",
       x="Trimestre/Año", y="Tasa") +
  geom_text(data = df_labels,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            size = 5, color = "black", family = "font") +
  geom_segment(data = df_segments,
               aes(x = x, xend = xend, y = y, yend = yend),
               inherit.aes = FALSE,
               color = "grey", linewidth = 0.25) +
  facet_wrap(~Indicador, nrow=2, ncol=2, dir="v", scales="free") +
  theme_light() +
  coord_cartesian(ylim = c(-10, 80), xlim=c(0.75, 6), clip="off", expand=FALSE) +
  scale_y_continuous(labels = function(z) paste0(formatC(z, big.mark = ".", decimal.mark=",", format="fg"), "%")) +
  scale_x_discrete(labels = function(z) paste0(str_sub(z, 6, -1), "°")) +
  scale_color_manual(name="Género", values=Colores) +
  theme(text=element_text(family="font"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=10, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(size=10, margin = margin(t=0,r=5,b=0,l=5)),
        axis.title.x = element_text(size=20, margin=margin(t=40)),
        axis.title.y = element_text(size=20),
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
       plot=grafico, dpi=100, width=10, height=8)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=8)

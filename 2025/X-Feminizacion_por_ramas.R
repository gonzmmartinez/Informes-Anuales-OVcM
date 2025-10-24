# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)
keep_attr <- function(.f, x, ...) {
  x_att <- attributes(x)
  res <- .f(x, ...)
  attributes(res) <- x_att
  res
}

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
Raw <- read.csv(file=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Datos_EPH.csv")) %>%
  filter(ANO4 == 2025, TRIMESTRE == 1)

# Filtrar datos
Data1 <- Raw %>%
  filter(ESTADO == 1, AGLOMERADO == 23, CH06 >= 14) %>%
  organize_labels() %>%
  organize_caes()

# Procesar datos
Resultados1 <- Data1 %>%
  mutate(P21 = as.numeric(P21)) %>%
  filter(P21 != "0") %>%
  group_by(caes_seccion_label) %>%
  summarise(Ingreso_mediano = keep_attr(median, P21, na.rm=TRUE)) %>%
  ungroup %>%
  rename(Actividad = "caes_seccion_label") %>%
  mutate(Actividad = str_to_sentence(Actividad))

Resultados2 <- calculate_tabulates(
  base = Data1,
  x="caes_seccion_label",
  y="CH04",
  weights = "PONDERA",
  digits = 2,
  add.percentage = "row"
) %>%
  select(-Varon) %>%
  rename(Tasa_feminizacion = "Mujer") %>%
  rename(Actividad = "caes_seccion_label/CH04") %>%
  mutate(Actividad = str_to_sentence(Actividad))

Data <- Resultados1 %>%
  left_join(Resultados2, by="Actividad") %>%
  filter(Actividad %in% c("Construcción",
                          "Información y comunicación",
                          "Transporte y almacenamiento",
                          "Explotación de minas y canteras",
                          "Actividades administrativas y servicios de apoyo",
                          "Industria manufacturera",
                          "Actividades inmobiliarias",
                          "Enseñanza",
                          "Salud humana y servicios sociales",
                          "Actividades de los hogares como empleadores de personal doméstico; actividades de los hogares como productores de bienes o servicios para uso propio",
                          "Alojamiento y servicios de comidas",
                          "Actividades financieras y de seguros",
                          "Administración pública y defensa",
                          "Agricultura, ganadería, caza, silvicultura y pesca")) %>%
  mutate(Actividad = ifelse(Actividad == "Actividades de los hogares como empleadores de personal doméstico; actividades de los hogares como productores de bienes o servicios para uso propio",
                            "Actividades de los hogares", Actividad))

# Colores
Paleta <- c("#5fad56", "#f2c14e", "#f78154", "#4d9078", "#b4436c")
Paleta2 <- c("#474E93", "#7E5CAD", "#b4436c", "#72BAA9", "#D5E7B5")

Colores <- c("Varones" = "#7E5CAD",
             "Mujeres" = "#72BAA9")

# Grafico
grafico <- ggplot(Data, aes(x=Tasa_feminizacion, y=Ingreso_mediano)) +
  annotate(geom="rect", xmin=1, xmax=45, ymin=300000, ymax=1480000, fill="grey", alpha=0.15, color=NA) +
  annotate(geom="rect", xmin=50, xmax=108, ymin=100000, ymax=900000, fill="grey", alpha=0.15, color=NA) +
  geom_point(aes(color=Tasa_feminizacion), size=5) +
  geom_text_repel(aes(label=str_wrap(Actividad, 25)), size=2.5, family="font", color="black", alpha=0.75,
                  point.padding = 10, segment.color = NA) +
  annotate(geom="text", x=56, y=1250000, label="ramas menos feminizadas\nmayores ingresos promedios",
           family="font", color="black", size=3, hjust=0.5) +
  annotate(geom="text", x=80, y=1000000, label="ramas más feminizadas\nmenores ingresos promedios",
           family="font", color="black", size=3, hjust=0.5) +
  scale_color_gradient2(high="#b4436c", low="#474E93", mid="#b4436c", midpoint = 60) +
  scale_y_continuous(labels = function(z) paste0("$", formatC(z, big.mark = ".", decimal.mark = ",", format="d")),
                     limits = c(0, 1500000), breaks=seq(0,1500000,by=250000), expand = c(0, 0)) +
  scale_x_continuous(labels = function(z) paste0(z, "%"), expand = c(0, 0), limits=c(0,110)) +
  labs(x="Tasa de feminización", y="Ingreso promedio del sector") +
  theme_linedraw() +
  theme(text=element_text(family="font"),
        legend.position="none",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=12, family="font"),
        legend.key.spacing.x = unit(0.5, "cm"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color="lightgrey"),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font"),
        axis.title.y = element_text(size=15, family="font"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=6)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=6)
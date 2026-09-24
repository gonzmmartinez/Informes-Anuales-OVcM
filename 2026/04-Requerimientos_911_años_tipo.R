# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- function(x, table) !(x %in% table)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add("font_title",    file.path(dir, "CreatoDisplay-ExtraBold.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_body",     file.path(dir, "RobotoSlab-Regular.ttf"))
font_add("font_body_b",     file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Años
Año_1 <- 2026
Año_2 <- 2025

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

# Año anterior
Data <- Raw %>%
  filter(Tipo %ni% c("Abuso sexual", "Abuso sexual (tentativa)"),
         Año %in% c(Año_2, Año_1)) %>%
  mutate(Accion = factor(case_when(Accion == "Llamadas" ~ "Llamadas recibidas por el 911",
                                   Accion == "Intervenciones" ~ "Intervenciones por agencia policial",
                                   Accion == "Intervenciones SAMEC" ~ "Intervenciones conjuntas con agencia SAMEC",
                                   Accion == "Llamadas SAMEC" ~ "Llamadas recibidas por el 911"),
                         levels=c("Llamadas recibidas por el 911","Intervenciones por agencia policial","Intervenciones conjuntas con agencia SAMEC")),
         Tipo = factor(Tipo, levels=c("Violencia de género", "Violencia de género histórica",
                                      "Violencia familiar en curso", "Violencia familiar histórica"))) %>%
  group_by(Año,Accion, Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Año = factor(case_when(Año == 2025 ~ "2025 (todo el año)",
                                Año == 2026 ~ "2026 (primer semestre)"),
                      levels=c("2026 (primer semestre)", "2025 (todo el año)"))) %>%
  group_by(Año, Accion) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup %>%
  mutate(Label_porcentajes = ifelse(Accion == "Llamadas recibidas por el 911" & Porcentaje > 10, paste0(formatC(round(Porcentaje, 1), format="fg", big.mark=".", decimal.mark=","), "%"), NA))

Title_ypos <- round(max(Data$Cantidad) * 1.4, -3)

Totales <- Data %>%
  group_by(Año,Accion) %>%
  summarise(Total = sum(Cantidad)) %>%
  ungroup %>%
  mutate(x = 1, y=Title_ypos)

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

# Definir colores
Colores <- c("Violencia de género" = "#c72a29",
             "Violencia de género histórica" = "#ec6230",
             "Violencia familiar en curso" = "#7c428a",
             "Violencia familiar histórica" = "#A648E0")

# Gr?fico
grafico <- ggplot(Data, aes(x=Accion, y=Cantidad, fill=Tipo)) +
  geom_col(position="dodge") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg"), group=Tipo),
            position=position_dodge(width=0.9), vjust=-0.5, size=5, family="font_body", color="black") +
  geom_text(aes(label=Label_porcentajes),
            position = position_dodge(width=0.9), vjust=1.75, size=4, family="font_body_b",
            color="white") +
  facet_wrap(~Año, nrow=2, scales='free') +
  geom_text(data=Totales, aes(x=Accion, y=y, label=formatC(Total, big.mark=".", decimal.mark=",", format="fg")),
            inherit.aes = FALSE, size=10, family="font_title", fontface="bold") +
  labs(title="",
       x="Requerimiento", y="Cantidad de requerimientos solicitados") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark=",", format="fg"),
                     limits = c(0, round(max(Data$Cantidad) * 1.5, -3))) +
  scale_fill_manual(name = str_wrap("Motivo del requerimiento", width=20),
                    labels = function(x) str_wrap(x, width = 20), values=Colores) +
  theme_light() +
  theme(text=element_text(family="font_body"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=12, family="font_title", margin=margin(r=20)),
        legend.text = element_text(size=12, family="font_subtitle"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_subtitle", size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_subtitle", size=10, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(family="font_subtitle", size=20),
        axis.title.y = element_text(family="font_subtitle", size=20),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=20, color="black", family="font_title", face="bold", margin=margin(t=10, b=10)))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=10)

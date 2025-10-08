# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1fX8iWndJKs_UTTcB1SoU5tpTK7ysVvxJeyVAE0C5gro/edit?usp=sharing",
                  sheet = "Mes")

# Año anterior
Data <- Raw %>%
  filter(Tipo != "Abuso sexual", Año %in% c(2024,2025)) %>%
  mutate(Accion = factor(case_when(Accion == "Llamadas" ~ "Llamadas recibidas por el 911",
                                   Accion == "Intervenciones" ~ "Intervenciones por agencia policial",
                                   Accion == "Intervenciones SAMEC" ~ "Intervenciones conjuntas con agencia SAMEC"),
                         levels=c("Llamadas recibidas por el 911","Intervenciones por agencia policial","Intervenciones conjuntas con agencia SAMEC")),
         Tipo = factor(Tipo, levels=c("Violencia de género", "Violencia familiar en curso", "Violencia familiar histórica"))) %>%
  group_by(Año,Accion, Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  ungroup %>%
  mutate(Año = factor(case_when(Año == 2024 ~ "2.024 (todo el año)",
                                Año == 2025 ~ "2.025 (primer semestre)"),
                      levels=c("2.025 (primer semestre)", "2.024 (todo el año)")))

Title_ypos <- round(max(Data$Cantidad) * 1.4, -3)

Totales <- Data %>%
  group_by(Año,Accion) %>%
  summarise(Total = sum(Cantidad)) %>%
  ungroup %>%
  mutate(x = 1, y=Title_ypos)

# Colores
Colores <- c("#7149C6", "#FC2947","#FE6244")

# Definir colores
Colores <- c("Violencia de género" = "#f2904c",
             "Violencia familiar en curso" = "#ec6489",
             "Violencia familiar histórica" = "#6e3169")

# Gr?fico
grafico <- ggplot(Data, aes(x=Accion, y=Cantidad, fill=Tipo)) +
  geom_col(position="dodge") +
  geom_text(aes(label=formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg"), group=Tipo),
            position=position_dodge(width=0.9), vjust=-0.5, size=7, family="font_sans", color="black") +
  facet_wrap(~Año, nrow=2, scales='free') +
  geom_text(data=Totales, aes(x=Accion, y=y, label=formatC(Total, big.mark=".", decimal.mark=",", format="fg")),
            inherit.aes = FALSE, size=10, family="font_sans", fontface="bold") +
  labs(title="",
       x="Requerimiento", y="Cantidad de requerimientos solicitados") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=20)) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark=",", format="fg"),
                     limits = c(0, round(max(Data$Cantidad) * 1.5, -3))) +
  scale_fill_manual(name = str_wrap("Motivo del requerimiento", width=20),
                    labels = function(x) str_wrap(x, width = 20), values=Colores) +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="bottom",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_text(size=15, family="font_sans"),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        strip.background = element_rect(color=NA, fill="#FE6244"),
        strip.text = element_text(size=25, color="white", family="font_serif", face="bold"))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=10)
ggsave(filename = paste0(filename, ".pdf"), path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=10)

# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
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

Data <- Raw %>%
  filter(Tipo != "Abuso sexual") %>%
  mutate(Tipo = ifelse(Tipo == "Violencia de género", Tipo, "Violencia familiar")) %>%
  mutate(Año = formatC(Año, big.mark=".", decimal.mark=",", format="fg"),
         Tipo = factor(Tipo, levels=c("Violencia familiar", "Violencia de género"))) %>%
  mutate(Año = as.factor(ifelse(Año %in% c("2.023", "2.025"), paste0(Año, "*"), Año))) %>%
  group_by(Año, Tipo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad/sum(Cantidad))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Definir colores
Colores <- c("Violencia de género" = "#ff621d",
             "Violencia familiar" = "#852f8c")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad, fill=Tipo)) +
  geom_hline(yintercept = 0.5, color="gray", linetype=2) +
  geom_col(position="fill", width=0.7) +
  geom_text(aes(label = paste0(formatC(round(Porcentaje,1), big.mark = ".", decimal.mark=",", format="fg"), "%")),
            family="font", position = position_fill(vjust = 0.5), size=7.5, color="white") +
  labs(y="Porcentaje", x="Año",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer trimestre únicamente.") +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  scale_fill_manual(name = str_wrap("Motivo del requerimiento", width=20),
                    values = Colores) +
  theme_light() +
  theme(text=element_text(family="font_sans"),
        legend.position="top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font_serif"),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=20, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))

# Guardar gr?fico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=7)
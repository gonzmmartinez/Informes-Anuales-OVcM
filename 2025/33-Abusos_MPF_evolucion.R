# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(directlabels)
library(googlesheets4)
library(lubridate)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/14m3Y-OdpPdvq0QLcFDZ7c0diTdoExJOHmOaY70qaeV8/edit?usp=sharing",
                  sheet = "Abuso")

Data <- Raw %>%
  filter(Año %in% c(2024, 2025)) %>%
  group_by(Año, Mes, Mes_ord) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  arrange(Año, Mes_ord) %>%
  mutate(Label = formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg")) %>%
  ungroup %>%
  mutate(Ord = row_number())

# Colores
Colores <- c("#6e3169", "#ec6489", "#FE6244")

# Gráfico
grafico <- ggplot(Data, aes(x=reorder(Label, Ord), y=Cantidad, group=1)) +
  geom_area(fill="#1daa6a", alpha=0.25) +
  geom_line(linewidth=2.5, color="#1daa6a", lineend = 'round') +
  geom_text(aes(label=Label), size=5, family="font_sans",
            fontface="bold", color="black", angle=90, hjust=1.5) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark=",", format="fg")) +
  scale_x_discrete(labels = c(str_to_title(month(1:12, label = TRUE, abbr = TRUE, locale = "es_ES")), 
                              str_to_title(month(1:6, label = TRUE, abbr = TRUE, locale = "es_ES")))) +
  labs(title="",
       x="Mes/Año", y="Cantidad") +
  annotate(geom="text", y=-50, x=c(6.5, 15.5), label=formatC(c(2024, 2025), big.mark=".", decimal.mark=",", format="d"),
           size=8, color="black", family="font_sans") +
  annotate(geom="segment", x=c(1,13,18), xend=c(1,13,18), y=-40, yend=-70, color="grey", linewidth=0.25) +
  theme_light() +
  coord_cartesian(ylim = c(-10, 350), xlim=c(0.75, 18.25), clip="off", expand=FALSE) +
  scale_alpha_continuous(range=c(0.5, 1)) +
  theme(text=element_text(family="font_sans"), legend.position="none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        panel.grid.major = element_line(colour = "#F5F5F5"),
        axis.text.x = element_text(size=15, margin = margin(t=5,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=20, margin=margin(t=40)),
        axis.title.y = element_text(size=20),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=14, height=7)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=14, height=7)
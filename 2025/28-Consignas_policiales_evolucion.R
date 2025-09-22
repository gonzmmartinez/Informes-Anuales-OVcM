# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rfuD4W7yQsjPiIXeAwo0Hh8nmHlg0NTDpIozghgGsGw/edit?usp=sharing",
                  sheet = "Consignas")

Data <- Raw %>%
  mutate(Año = formatC(Año, big.mark = ".", decimal.mark = ",", format="fg")) %>%
  group_by(Año, Tipo) %>%
  summarise(Total = sum(Cantidad)) %>%
  mutate(Tipo = factor(Tipo, levels=c("Fija", "Ambulatoria", "Personalizada"))) %>%
  mutate(Año = ifelse(Año == "2.025", "2.025*", Año))

Totales <- Data %>%
  group_by(Año) %>%
  summarise(Total = sum(Total))

# Definir colores
Colores <- c("Fija" = "#ec6489",
             "Ambulatoria" = "#f2904c",
             "Personalizada" = "#72bf90")

# Grafico 1
grafico <- ggplot(Data, aes(x=Año, y=Total, fill=Tipo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=formatC(Total, big.mark=".", decimal.mark=",", format="fg")),
            position = position_stack(vjust=1), vjust=-0.5, size=3, family="font", color="black") +
  geom_text(data=Totales, inherit.aes = FALSE,
            aes(x=Año, y=35000, label=formatC(Total, big.mark = ".", decimal.mark = ",", format="fg")),
            family="font", fontface="bold", size=6) +
  theme_light() +
  labs(y="Cantidad",
       caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  scale_fill_manual(name="Tipo de consigna", values = Colores) +
  scale_y_continuous(labels = function(z) formatC(z, big.mark = ".", decimal.mark = ",", format="fg")) +
  theme(text=element_text(family="font"),
        legend.position = "top",
        legend.justification = "right",
        legend.title = element_text(size=10, family="font"),
        legend.text = element_text(size=12, family="font"),
        plot.title = element_text(size=20, family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font"),
        plot.caption = element_text(size=7.5, family="font", face="italic", margin=margin(t=10)),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=15, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=15, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=8, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=8, height=5)
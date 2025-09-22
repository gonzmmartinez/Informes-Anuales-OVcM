# Limpiar todo
rm(list = ls())

# Librerías
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rfuD4W7yQsjPiIXeAwo0Hh8nmHlg0NTDpIozghgGsGw/edit?usp=sharing",
                  sheet = "Consignas")

# Definir colores
Colores <- c("#e54c7c", "#1daa6a")

# Modificar datos
Data <- Raw %>%
  group_by(Año, Sujeto) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = round(100 * Cantidad/sum(Cantidad),1)) %>%
  ungroup() %>%
  mutate(Label = paste0("<span style='font-size:15pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                        "%**</span><br><span style='font-size:10pt'>",
                        formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg"),
                        "</span>")) %>%
  group_by(Año) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Año = formatC(Año, big.mark = ".", decimal.mark = ",", format="fg")) %>%
  mutate(Año = ifelse(Año == "2.025", "2.025*", Año))

# Grafico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sujeto)) +
  geom_rect() +
  facet_wrap(~Año, nrow=1) +
  coord_polar(theta="y") +
  theme_void() +
  labs(caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  geom_richtext(aes(x=4, y=ymid, label = Label), color = "black", label.color = NA,
                family="font", show.legend=FALSE, fill=NA, nudge_x=1, size=4) +
  geom_text(aes(x=1, y=0, label=Año), size=7.5, family="font", fontface="bold", color="black") +
  xlim(1,5) +
  scale_fill_manual(name=str_wrap("Destinatario de la consigna", width=15), values=Colores[c(1,2)]) +
  theme(text=element_text(family="font", size=20),
        legend.position="bottom",
        legend.justification = "center",
        legend.margin = margin(t=20),
        legend.title = element_text(family="font", size=10, margin=margin(r=15)),
        legend.key.spacing.x = unit(0.5, "cm"),
        legend.text = element_text(family="font", size=12),
        plot.margin = margin(t=0,r=0,b=0,l=0),
        plot.background = element_rect(fill="white", color=NA),
        plot.caption = element_text(size=8, family="font", face="italic", margin=margin(t=20)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(-1, "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=3.5)
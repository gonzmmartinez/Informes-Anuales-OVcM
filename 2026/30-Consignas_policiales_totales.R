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
library(sysfonts)
library(showtext)
dir <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1rfuD4W7yQsjPiIXeAwo0Hh8nmHlg0NTDpIozghgGsGw/edit?usp=sharing",
                  sheet = "Consignas")

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c("Persona denunciada" = "#c72a29",
             "Víctima" = "#119ca0")

# Modificar datos
Data <- Raw %>%
  group_by(Año, Sujeto) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Porcentaje = round(100 * Cantidad/sum(Cantidad),1)) %>%
  ungroup() %>%
  mutate(Label = paste0("<span style='font-size:12.5pt'>**",
                        formatC(round(Porcentaje,1), big.mark=".", decimal.mark=",", format="fg"),
                        "%**</span><br><span style='font-size:8pt'>",
                        formatC(Cantidad, big.mark=".", decimal.mark=",", format="fg"),
                        "</span>")) %>%
  group_by(Año) %>%
  mutate(ymax = cumsum(Porcentaje)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  rowwise() %>%
  mutate(ymid = ymax - (ymax - ymin)/2) %>%
  ungroup() %>%
  mutate(Año = as.character(Año)) %>%
  mutate(Año = ifelse(Año == "2026", "2026*", Año)) %>%
  mutate(Sujeto = ifelse(Sujeto == "Víctima", "Víctima", "Persona denunciada"))

# Grafico
grafico <- ggplot(Data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Sujeto)) +
  geom_rect() +
  facet_wrap(~Año, nrow=1) +
  coord_polar(theta="y") +
  theme_void() +
  labs(caption="* las proporciones se calculan en base a los datos correspondientes al primer semestre únicamente.") +
  geom_richtext(aes(x=4, y=ymid, label = Label), color = "black", label.color = NA,
                family="font_body", show.legend=FALSE, fill=NA, nudge_x=1, size=4, lineheight = 0.9) +
  geom_text(aes(x=1, y=0, label=Año), size=6, family="font_title", fontface="bold", color="black") +
  xlim(1,5) +
  scale_fill_manual(name="Destinatario de la consigna", values=Colores) +
  theme(text=element_text(family="font_body", size=20),
        legend.position="bottom",
        legend.justification = "center",
        legend.margin = margin(t=10),
        legend.title = element_text(family="font_title", face="bold", size=12, margin=margin(r=15)),
        legend.key.spacing.x = unit(0.5, "cm"),
        legend.text = element_text(family="font_title", size=12),
        plot.margin = margin(t=0,r=0,b=0,l=0),
        plot.background = element_rect(fill="white", color=NA),
        plot.caption = element_text(size=8, family="font_title", face="italic", margin=margin(t=20)),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(-1.5, "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=3)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=3)
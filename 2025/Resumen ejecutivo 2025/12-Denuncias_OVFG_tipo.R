# Limpiar todo
rm(list = ls())

# Librer?as
library(ggplot2)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(ggtext)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Tipo") %>%
  mutate(Tipo = ifelse(Tipo == "Económica/Patrimonial", "Económica/ Patrimonial", Tipo))

Data <- Raw %>%
  filter(Año == 2025, Tipo != "Sin especificar") %>%
  group_by(Año, Tipo) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup()

Data <- Data %>%
  filter(Tipo != "Otros") %>%
  arrange(desc(Porcentaje)) %>%
  mutate(xpos = c(1, 2, 1, 2, 1.5)) %>%
  mutate(ypos = c(2, 2, 1.5, 1.5, 1))

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Física" = "#206170",
             "Psicológica" = "#ff621d",
             "Simbólica" = "#2b42a0",
             "Económica/ Patrimonial" = "#a782ec",
             "Sexual" = "#d3335e")
# Gráfico1
grafico <- ggplot(Data, aes(x=xpos, y=ypos, color=Tipo)) +
  geom_point(aes(size=Porcentaje), shape = 15) +
  geom_text(aes(label=paste0(formatC(Porcentaje, digits=2, big.mark=".", decimal.mark=",", format="f"), "%")),
            color="black", size=7.5, family="font_sans", fontface="bold", nudge_x=0.35, nudge_y=-0.05) +
  geom_text(aes(label=str_wrap(Tipo, 15)), color="black", size=5, family="font_sans",
            nudge_x=0.35, nudge_y=0.05, lineheight=0.75, vjust=0) +
  theme_void() +
  scale_size_continuous(range=c(2, 30)) +
  xlim(0.75, 2.5) +
  ylim(0.9, 2.1) +
  scale_color_manual(values=Colores) +
  theme(text=element_text(family="font_sans"),
        legend.position = "none",
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        legend.key.spacing.y = unit(0.25, "cm"),
        plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=6, height=3.5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=6, height=3.5)


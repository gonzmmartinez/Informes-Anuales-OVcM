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
                  sheet = "Modalidad")

Data <- Raw %>%
  filter(Año == 2025, Modalidad != "Sin especificar") %>%
  group_by(Año, Modalidad) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup()

Data <- Data %>%
  filter(Modalidad != "Otras") %>%
  arrange(desc(Porcentaje)) %>%
  mutate(xpos = c(rep(c(1,2), 3), 1)) %>%
  rbind(c(Data %>% filter(Modalidad == "Otras"), xpos=2)) %>%
  mutate(ypos = rep(c(2.5, 2, 1.5, 1), each=2))

# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("Doméstica" = "#852f8c",
             "Acoso callejero" = "#a782ec",
             "Institucional" = "#ff9d27",
             "Laboral" = "#5ec5d4",
             "Mediática" = "#2b42a0",
             "Obstétrica" = "#206170",
             "Política" = "#f93e35",
             "Otras" = "#cbc2ce")

# Gráfico1
grafico <- ggplot(Data, aes(x=xpos, y=ypos, color=Modalidad)) +
  geom_point(aes(size=Porcentaje), shape = 15) +
  geom_text(aes(label=paste0(formatC(Porcentaje, digits=2, big.mark=".", decimal.mark=",", format="f"), "%")),
            color="black", size=7.5, family="font_sans", fontface="bold", nudge_x=0.35, nudge_y=-0.075) +
  geom_text(aes(label=Modalidad), color="black", size=5, family="font_sans", nudge_x=0.35, nudge_y=0.075) +
  theme_void() +
  scale_size_continuous(range=c(2, 30)) +
  xlim(0.9, 2.5) +
  ylim(0.8, 2.7) +
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
       plot=grafico, dpi=100, width=6, height=4)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=6, height=4)


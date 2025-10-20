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
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Cfbecjc5DLo3uGsMEHscsfUC9YOtnKtFvt1bOZI_B4c/edit?usp=sharing",
                  sheet = "Modalidad")

Data <- Raw %>%
  filter(Año == 2025, Modalidad != "Sin especificar") %>%
  mutate(Modalidad = factor(Modalidad, levels=Levels)) %>%
  group_by(Año, Modalidad) %>%
  summarise(Cantidad = sum(Frecuencia)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup()

Data <- Data %>%
  filter(Modalidad != "Otras") %>%
  arrange(desc(Porcentaje)) %>%
  mutate(xpos = c(rep(c(1,2), 3), 1)) %>%
  rbind(c(Data %>% filter(Modalidad == "Otras"), xpos=2)) %>%
  mutate(ypos = rep(4:1, each=2))

# Definir colores
Colores <- c("Doméstica" = "#e54c7c",
             "Acoso callejero" = "#6e3169",
             "Institucional" = "#f2904c",
             "Laboral" = "#ffd241",
             "Mediática" = "#1daa6a",
             "Obstétrica" = "#4cb2f2",
             "Política" = "#f24c7e",
             "Otras" = "#747264")

# Gráfico1
grafico <- ggplot(Data, aes(x=xpos, y=ypos, color=Modalidad)) +
  geom_point(aes(size=Porcentaje), shape = 15) +
  geom_text(aes(label=paste0(formatC(Porcentaje, digits=2, big.mark=".", decimal.mark=",", format="f"), "%")),
            color="black", size=7.5, family="font", fontface="bold", nudge_x=0.35, nudge_y=-0.1) +
  geom_text(aes(label=Modalidad), color="black", size=5, family="font", nudge_x=0.35, nudge_y=0.1) +
  theme_void() +
  scale_size_continuous(range=c(2, 30)) +
  xlim(0.75, 2.5) +
  ylim(0.5, 4.5) +
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
       plot=grafico, dpi=100, width=6, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=6, height=6)


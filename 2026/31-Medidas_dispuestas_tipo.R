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
library(ggbump)

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
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1AO8SmJ45quqCCvg9nZ0vZqRLvxZMfAqB7uzOtXmu9Ro/edit?usp=sharing",
                  sheet = "Medidas")

Data0 <- Raw %>%
  group_by(Año, Medida) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup

Medidas <- c(
  Data0 %>% filter(Año == 2023) %>% slice_max(Cantidad, n = 15) %>% pull(Medida),
  Data0 %>% filter(Año == 2024) %>% slice_max(Cantidad, n = 15) %>% pull(Medida),
  Data0 %>% filter(Año == 2025) %>% slice_max(Cantidad, n = 15) %>% pull(Medida),
  Data0 %>% filter(Año == 2026) %>% slice_max(Cantidad, n = 15) %>% pull(Medida)
) %>%
  unique()

Data0 <- Data0 %>%
  mutate(Medida = ifelse(Medida %ni% Medidas, "Otras", Medida)) %>%
  group_by(Año, Medida) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup

Data <- Data0 %>%
  filter(Medida != "Otras") %>%
  filter(Medida != "Competencia",
         Porcentaje >= 0.1) %>%
  arrange(Año, desc(Porcentaje)) %>%
  group_by(Año) %>%
  mutate(Level = row_number()) %>%
  filter(Level != "15") %>%
  rbind(Data0 %>% filter(Medida == "Otras")) %>%
  mutate(Level = ifelse(Medida == "Otras", 15, Level)) %>%
  ungroup %>%
  mutate(Level = formatC(Level, width=2, flag="0"),
         Año = as.character(Año)) %>%
  mutate(Axis = case_when(Año == "2023" ~ "<span style='font-size:20pt'>**2023**</span><br><span style='font-size:15pt'>*Todo el año*</span>",
                          Año == "2024" ~ "<span style='font-size:20pt'>**2024**</span><br><span style='font-size:15pt'>*Todo el año*</span>",
                          Año == "2025" ~ "<span style='font-size:20pt'>**2025**</span><br><span style='font-size:15pt'>*Todo el año*</span>",
                          Año == "2026" ~ "<span style='font-size:20pt'>**2026**</span><br><span style='font-size:15pt'>*Primer semestre*</span>"))

# Colores
Paleta <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
            "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- c(
  "Abstenerse de ejercer actos de violencia" = "#ec6230",
  "Consigna policial ambulatoria" = "#e0502e",
  "Prohibición de acercamiento y/o contacto" = "#d43d2b",
  "Consigna policial fija" = "#c72b29",
  "Informes de OVFG y otros tipos de informes" = "#a02738",
  "Exclusión del hogar" = "#772447",
  "Tratamiento psicológico" = "#4c2158",
  "Intervención Secretaria de primera infancia Niñez y Familia" = "#5c2c69",
  "Competencia" = "#6c3779",
  "Informe constatación con concepto vecinal" = "#7c428a",
  "Provisión de dispositivos de alarma" = "#764d94",
  "Prohibición de contacto por medios de comunicación" = "#6f58a0",
  "Prohibición de castigos conforme art. 647 C.C.C.N." = "#6a64aa",
  "Consigna personalizada para la victima" = "#848aab",
  "Intima a cumplir deberes inherentes a la resp. parental" = "#9eb0ab",
  "Consigna personalizada para el denunciado" = "#b8d6ac",
  "Reintegro al domicilio" = "#7dc2a8",
  "Medidas administrativas" = "#46aea4",
  "Suspension del régimen de comunicación" = "#119c9f",
  "Cuidado personal unilateral provisorio" = "#15917b",
  "Fijación alimentos provisorios" = "#1a8658",
  "Otras" = "#1e7b34"
)

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Level, color=Medida, group=Medida)) +
  geom_bump(linewidth = 1.5) +
  geom_label(aes(label=paste0(formatC(round(Porcentaje,1), format="fg", big.mark=".", decimal.mark=","), "%"),
                 size=Porcentaje, fill=Medida), family="font_body", fontface="bold", color="white") +
  geom_text(aes(label=formatC(Cantidad, big.mark = ".", decimal.mark=",", format="fg")),
            size=3, color="grey10", family="font_title", hjust=0.5, nudge_y=-0.4) +
  geom_text(data=Data %>% filter(Año == "2023"), aes(x=1-0.25, y=Level, label=str_wrap(Medida, width=25)),
            color="black", size=3, family="font_title", hjust=1, lineheight = 0.9) +
  geom_text(data=Data %>% filter(Año == "2024", Level %in% c("09", "11", "12", "13", "14")),
            aes(x=2-0.15, y=Level, label=str_wrap(Medida, width=20)),
            color="black", size=3, family="font_title", hjust=1, lineheight = 0.9) +
  geom_text(data=Data %>% filter(Año == "2026"), aes(x=4+0.25, y=Level, label=str_wrap(Medida, width=25)),
            color="black", size=3, family="font_title", hjust=0, lineheight = 0.9) +
  scale_y_discrete(limits = rev(sprintf("%02d", 1:15))) +
  scale_x_discrete(expand = c(0.3,0.3), position = "top", labels = function(x) Data$Axis[match(x, Data$Año)]) +
  scale_size_continuous(range=c(3, 6)) +
  scale_color_manual(values=Colores) +
  scale_fill_manual(values=Colores) +
  labs(x="Año", y="Medidas dispuestas") +
  theme_light() +
  theme(legend.position="none",
        plot.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color="grey90"),
        axis.title.y = element_text(family="font_title", size=20, margin=margin(r=10)),
        axis.title.x.top = element_text(family="font_title", size=20, margin=margin(t=0, r=0, b=15, l=0)),
        axis.text.y = element_blank(),
        axis.text.x.top = element_markdown(family="font_title", size=15, margin=margin(b=10), lineheight = 1),
        axis.ticks.y = element_blank())

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=10)


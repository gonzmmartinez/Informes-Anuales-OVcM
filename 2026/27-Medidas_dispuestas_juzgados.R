# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(ggplot2)
library(ggbump)
library(ggtext)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)
library(tidyr)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1AO8SmJ45quqCCvg9nZ0vZqRLvxZMfAqB7uzOtXmu9Ro/edit?usp=sharing",
                  sheet = "Medidas")

Data0 <- Raw %>%
  group_by(Año, Medida) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup

Medidas <- Data0 %>%
  filter(Año == 2025) %>%
  arrange(desc(Cantidad)) %>%
  top_n(15) %>%
  pull(Medida)

Data0 <- Data0 %>%
  mutate(Medida = ifelse(Medida %ni% Medidas, "Otras", Medida)) %>%
  group_by(Año, Medida) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  ungroup

Data <- Data0 %>%
  filter(Medida != "Otras") %>%
  filter(Medida != "Competencia") %>%
  arrange(Año, desc(Porcentaje)) %>%
  group_by(Año) %>%
  mutate(Level = row_number()) %>%
  rbind(Data0 %>% filter(Medida == "Otras")) %>%
  mutate(Level = ifelse(Medida == "Otras", 14, Level)) %>%
  ungroup %>%
  mutate(Level = formatC(Level, width=2, flag="0"),
         Año = as.character(Año)) %>%
  mutate(Axis = case_when(Año == "2023" ~ "<span style='font-size:20pt'>**2.023**</span><br><span style='font-size:15pt'>*Todo el año*</span>",
                          Año == "2024" ~ "<span style='font-size:20pt'>**2.024**</span><br><span style='font-size:15pt'>*Todo el año*</span>",
                          Año == "2025" ~ "<span style='font-size:20pt'>**2.025**</span><br><span style='font-size:15pt'>*Primer semestre*</span>"))

# Definir colores
Colores <- c("#ff621d", "#206170", "#a782ec", "#852f8c", "#0f216d", "#2b42a0", "#386020",
             "#ff9d27", "#f93e35", "#d3335e", "#5ec5d4", "#839936", "#3348d3", "#d33383", "#f93e35")

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Level, color=Medida, group=Medida)) +
  geom_bump(linewidth = 1.5) +
  geom_label(aes(label=paste0(formatC(round(Porcentaje,1), format="fg", big.mark=".", decimal.mark=","), "%"),
                 size=Porcentaje, fill=Medida), family="font_sans", fontface="bold", color="white") +
  geom_text(aes(label=formatC(Cantidad, big.mark = ".", decimal.mark=",", format="fg")),
            size=3, color="grey10", family="font_sans", hjust=0.5, nudge_y=-0.4) +
  geom_text(data=Data %>% filter(Año == "2023"), aes(x=1-0.2, y=Level, label=str_wrap(Medida, width=25)),
            color="black", size=3.5, family="font_sans", hjust=1, lineheight = 1) +
  geom_text(data=Data %>% filter(Año == "2025"), aes(x=3+0.2, y=Level, label=str_wrap(Medida, width=25)),
            color="black", size=3.5, family="font_sans", hjust=0, lineheight = 1) +
  geom_text(data=Data %>% filter(Año == "2024", Level %in% c("09", "11", "12", "13")),
            aes(x=2-0.1, y=Level, label=str_wrap(Medida, width=25)),
            color="black", size=3.5, family="font_sans", hjust=1, lineheight = 1) +
  scale_y_discrete(limits = rev) +
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
        axis.title.y = element_text(family="font_sans", size=20, margin=margin(r=10)),
        axis.title.x.top = element_text(family="font_sans", size=20, margin=margin(t=0, r=0, b=15, l=0)),
        axis.text.y = element_blank(),
        axis.text.x.top = element_markdown(family="font_serif", size=15, margin=margin(b=10), lineheight = 1),
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
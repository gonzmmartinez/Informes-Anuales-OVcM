# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librer?as
library(ggplot2)
library(ggbump)
library(dplyr)
library(stringr)
library(cowplot)
library(magick)
library(googlesheets4)

# Fuentes
library(showtext)
font_add_google("Barlow", "font")
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
  arrange(Año, desc(Cantidad)) %>%
  group_by(Año) %>%
  mutate(Level = row_number()) %>%
  rbind(Data0 %>% filter(Medida == "Otras")) %>%
  mutate(Level = ifelse(Medida == "Otras", 15, Level)) %>%
  ungroup %>%
  mutate(Level = formatC(Level, width=2, flag="0"),
         Año = as.character(Año))

# Gráfico
grafico <- ggplot(Data, aes(x=Año, y=Level, color=Medida, group=Medida)) +
  geom_bump(linewidth = 1.5) +
  geom_label(aes(label=formatC(Cantidad, format="fg", big.mark=".", decimal.mark=","), size=Cantidad, fill=Medida),
             family="font", fontface="bold", color="white") +
  geom_text(data=Data %>% filter(Año == "2023"), aes(x=1-0.15, y=Level, label=str_wrap(Medida, width=25)),
            color="black", size=3.5, family="font", hjust=1, lineheight = 1) +
  geom_text(data=Data %>% filter(Año == "2025"), aes(x=3+0.15, y=Level, label=str_wrap(Medida, width=25)),
            color="black", size=3.5, family="font", hjust=0, lineheight = 1) +
  scale_y_discrete(limits = rev) +
  scale_x_discrete(expand = c(0.3,0.3)) +
  scale_size_continuous(limits=c(3,5)) +
  theme_void() +
  theme(legend.position="none",
        plot.background = element_rect(fill="white", color=NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=10)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=10)
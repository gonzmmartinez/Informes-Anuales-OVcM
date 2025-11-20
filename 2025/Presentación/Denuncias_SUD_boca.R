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
library(scales)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa") %>%
  filter(Tipo != "Penal")

margen <- 15

Data <- Raw %>%
  filter(Año == 2025) %>%
  group_by(Organismo) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  mutate(Porcentaje = 100 * Cantidad / sum(Cantidad)) %>%
  mutate(Organismo = ifelse(Organismo == "OOD", "OOyD", Organismo)) %>%
  ungroup() %>%
  arrange(desc(Porcentaje)) %>%
  mutate(
    Organismo = forcats::fct_reorder(Organismo, Porcentaje, .desc = TRUE),
    
    # --- calcular radio equivalente al size_area(max_size = 80) ---
    area  = scales::rescale(Porcentaje, to = c(0, 80)),
    radio = sqrt(area / pi),
    
    # --- posiciones acumuladas en x según el radio ---
    x_raw = cumsum(lag(radio, default = 0) + radio + margen),
    
    # --- reescalar x_raw al rango 1–4 ---
    x = scales::rescale(x_raw, to = c(1, 4)),
    
    # tus nudges
    nudge_y = rescale(sqrt(Porcentaje), to = c(1.2, 1.7)),
    nudge_y_text = rescale(sqrt(Porcentaje), to = c(0.8, 0.3))
  )



# Definir colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("OVFG" = "#2b42a0",
             "OOyD" = "#f93e35",
             "Fiscalías" = "#ff9d27",
             "Comisarías" = "#a782ec")

# Gráfico
# Gráfico
grafico <- ggplot(Data, aes(x=x, y=1)) +
  geom_point(aes(color=Organismo, size=Porcentaje)) +
  geom_text(aes(label=paste0(formatC(round(Porcentaje, 1), big.mark=".", decimal.mark = ",", format="fg"), "%"),
                y=nudge_y, color=Organismo), size=15, family="font_sans", fontface="bold") +
  geom_text(aes(label=Organismo, y=nudge_y_text), size=12, family="font_sans", color="grey20") +
  scale_size_area(max_size = 80) +
  scale_x_continuous(limits=c(0.5,4.25)) +
  scale_y_continuous(limits=c(0,2)) +
  scale_color_manual(values=Colores) +
  theme_void() +
  theme(legend.position = "none")


# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=5)


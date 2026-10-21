# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- function(x, table) !(x %in% table)

# Librerias
library(openxlsx)
library(tidyverse)
library(ggforce)
library(ggthemes)
library(devtools)
library(geogrid)
library(sf)
library(tmap)
library(ggspatial)
library(ggpp)
library(googlesheets4)
library(ggsflabel)

# Fuentes
library(sysfonts)
library(showtext)
dir <- paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Fonts/")
font_add(family = "font_title",
         bold = file.path(dir, "CreatoDisplay-ExtraBold.otf"),
         regular = file.path(dir, "CreatoDisplay-Regular.otf"))
font_add("font_subtitle", file.path(dir, "CreatoDisplay-Regular.otf"))
font_add(family = "font_body",
         regular = file.path(dir, "RobotoSlab-Regular.ttf"),
         bold = file.path(dir, "RobotoSlab-Bold.ttf"))
showtext_auto()

# Cargar shape
Mapa_Salta <- st_read(paste0(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)), "/Mapa_shape/Salta_deptos_shape/Salta_deptos.shp")) %>%
  mutate(etiqueta = case_when(etiqueta == "San Martín" ~ "General José de San Martín",
                              .default = etiqueta))

# Leer datos
Raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                  sheet = "SUD_db_completa")

Poblacion_raw <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1mUMxGbv3x1hoVxWbTfAquSDR25YWnSDTL8T5MFkllvU/edit?usp=sharing",
                            sheet = "Poblacion")

Data1 <- Raw %>%
  filter(Año %in% c(2025), Tipo %ni% c("Penal")) %>%
  group_by(Departamento) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  left_join(Poblacion_raw, by="Departamento") %>%
  filter(Año == 2025) %>% select(-Año) %>%
  mutate(Tasa = (100 * Cantidad.x) / Cantidad.y,
         Codigo = as.character(Codigo))

Mapa_Salta1 <- Mapa_Salta %>%
  left_join(
    Data1,
    by = c("cod_depto" = "Codigo")
  )

Mapa_labels1 <- Mapa_Salta1 %>%
  slice_max(order_by = Tasa, n = 5) %>%
  mutate(Order = row_number(),
         Label = paste0(Order, ". ", str_wrap(Departamento, 15), ":\n",
                        formatC(round(Tasa, 2), decimal.mark=",", format="fg")))

Data2 <- Raw %>%
  filter(Año %in% c(2026), Tipo %ni% c("Penal")) %>%
  group_by(Departamento) %>%
  summarise(Cantidad = sum(Cantidad)) %>%
  left_join(Poblacion_raw, by="Departamento") %>%
  filter(Año == 2026) %>% select(-Año) %>%
  mutate(Tasa = (100 * Cantidad.x) / Cantidad.y,
         Codigo = as.character(Codigo))

Mapa_Salta2 <- Mapa_Salta %>%
  left_join(
    Data2,
    by = c("cod_depto" = "Codigo")
  )

Mapa_labels2 <- Mapa_Salta2 %>%
  slice_max(order_by = Tasa, n = 5) %>%
  mutate(Order = row_number(),
         Label = paste0(Order, ". ", str_wrap(Departamento, 15), ":\n",
                        formatC(round(Tasa, 2), decimal.mark=",", format="fg")))

  
# Colores
Paleta2 <- c("#1e7b34", "#119ca0", "#b8d6ac", "#6963aa",
             "#7c428a", "#4c2158", "#c72a29", "#ec6230", "#cbc2ce")

Colores <- colorRampPalette(c("white", "#ec6230"))(3)

# Grafico
grafico1 <- ggplot(Mapa_Salta1) +
  geom_sf(color="white", aes(fill=Tasa), linewidth=0.5) +
  geom_sf(data=Mapa_labels1, color="black", fill=NA, linewidth=0.75) +
  ggsflabel::geom_sf_label_repel(data = Mapa_labels1, aes(label=Label), lineheight=0.9,
                                 color="black", family="font_title", size=3, show.legend=FALSE,
                                 force=10, force_pull=0.5, box.padding=1,
                                 max.iter=20000, max.overlaps=Inf, seed=123, fontface="bold") +
  labs(title = "2025 (todo el año)") +
  theme_void() +
  scale_fill_gradient(name = "Tasa de denuncias por cada 100 habitantes",
                       low = "#E7DBE9", high="#7c428a", na.value = "grey90") +
  guides(color = guide_legend(override.aes=list(shape = 16))) +
  theme(legend.title.position = "top",
        legend.title = element_text(family="font_title", size=10, face="bold", hjust=0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(family="font_body"),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.text = element_text(family="font_body", size=10),
        legend.key.spacing.y = unit(0, "cm"),
        plot.title = element_text(family="font_title", size=20,
                                  face="bold", hjust=0.5,
                                  margin=margin(b=-10)),
        plot.margin=unit(c(0.25,0.25,0.25,0.25), "cm"),
        plot.background = element_rect(fill="white", color=NA))

# Gráfico 2
grafico2 <- ggplot(Mapa_Salta2) +
  geom_sf(color="white", aes(fill=Tasa), linewidth=0.5) +
  geom_sf(data=Mapa_labels2, color="black", fill=NA, linewidth=0.75) +
  ggsflabel::geom_sf_label_repel(data = Mapa_labels2, aes(label=Label), lineheight=0.9,
                                 color="black", family="font_title", size=3, show.legend=FALSE,
                                 force=10, force_pull=0.5, box.padding=1,
                                 max.iter=20000, max.overlaps=Inf, seed=123, fontface="bold") +
  labs(title = "2026 (primer semestre)") +
  theme_void() +
  scale_fill_gradient(name = "Tasa de denuncias por cada 100 habitantes",
                      low = "#E7DBE9", high="#7c428a", na.value = "grey90") +
  guides(color = guide_legend(override.aes=list(shape = 16))) +
  theme(legend.title.position = "top",
        legend.title = element_text(family="font_title", size=10, face="bold", hjust=0.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        text = element_text(family="font_body"),
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(1.5, "cm"),
        legend.text = element_text(family="font_body", size=10),
        legend.key.spacing.y = unit(0, "cm"),
        plot.title = element_text(family="font_title", size=20,
                                  face="bold", hjust=0.5,
                                  margin=margin(b=-10)),
        plot.margin=unit(c(0.25,0.25,0.25,0.25), "cm"),
        plot.background = element_rect(fill="white", color=NA))

# Layout
grafico <- plot_grid(grafico1, grafico2, ncol=2,
                     rel_widths = c(1,1)) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=13, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=13, height=6)


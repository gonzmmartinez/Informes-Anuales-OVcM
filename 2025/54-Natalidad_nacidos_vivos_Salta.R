# Limpiar todo
rm(list = ls())

# Funciones
`%ni%` <- Negate(`%in%`)

# Librerías
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(googlesheets4)
library(purrr)
library(readxl)
library(ggtext)

# Fuentes
library(showtext)
font_add_google("Source Sans 3", "font_sans")
font_add_google("Source Serif 4", "font_serif")
showtext_auto()

# LEER DATOS DE NACIDOS VIVOS
# Ruta base
ruta_base <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/DEIS/")

# Años a cargar (de 2023 a 2005)
anios <- 2023:2005

# Función para cargar cada archivo y agregar columna AÑO
Raw <- map_dfr(anios, function(anio) {
  archivo <- paste0(ruta_base, "nacweb", substr(anio, 3, 4), ".csv")
  # Elegimos read_csv2 para años 2023 a 2020, read_csv para el resto
  lector <- if (anio >= 2020) read_csv2 else read_csv
  lector(file = archivo) %>%
    mutate(AÑO = anio)
})

# Leer códigos de provincias
Codigos_provincias <- read_excel(path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/DEIS/descnac.xlsx"),
                                 sheet="PROVRES") %>%
  rename(Codigo = "CODIGO", Provincia = "VALOR") %>%
  mutate(Provincia = ifelse(Provincia == "Ciudad Aut. de Buenos Aires", "Ciudad Autónoma de Buenos Aires", Provincia)) %>%
  add_row(Codigo = "01", Provincia = "Total del país")

# Modificar datos
Nacidos_vivos <- Raw %>%
  mutate(Edad = case_when(IMEDAD == "1.Menor de 15" ~ "Menos de 15 años",
                          IMEDAD == "2.15 a 19" ~ "15-19 años",
                          IMEDAD == "3.20 a 24" ~ "20-24 años",
                          IMEDAD == "4.25 a 29" ~ "25-29 años",
                          IMEDAD == "5.30 a 34" ~ "30-34 años",
                          IMEDAD == "6.35 a 39" ~ "35-39 años",
                          IMEDAD == "7.40 a 44" ~ "40-44 años",
                          IMEDAD == "8.De 45 y más" ~ "45 años o más",
                          IMEDAD == "9.Sin especificar" ~ "Sin especificar",
                          .default = "45 años o más")) %>%
  select(PROVRES, AÑO, Edad, CUENTA) %>%
  rename(Codigo = "PROVRES", Año = "AÑO", Cantidad = "CUENTA") %>%
  left_join(Codigos_provincias, by="Codigo") %>%
  select(Año, Provincia, Edad, Cantidad) %>%
  filter(Edad != "Sin especificar",
         Provincia %ni% c("Lugar no especificado", "Otro país")) %>%
  rename(Rango_etario = "Edad")

Data <- Nacidos_vivos %>%
  filter(Provincia == "Salta") %>%
  group_by(Año) %>%
  summarise(Cantidad = sum(Cantidad))

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

# Grafico
grafico <- ggplot(Data, aes(x=Año, y=Cantidad)) +
  geom_col(aes(fill=Cantidad)) +
  geom_text(aes(label=formatC(Cantidad, big.mark = ".", decimal.mark = ",", format="fg")),
            nudge_y=-1000, family="font_sans", fontface="bold", size=2.5, color="white") +
  scale_x_continuous(breaks = seq(from=2005, to=2025, by=2), expand = c(0.01, 0.01),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg")) +
  scale_y_continuous(labels = function(z) formatC(z, format = "fg", big.mark = ".", decimal.mark = ","),
                     breaks = seq(from=0, to=30000, by=5000)) +
  scale_fill_gradient(high="#a782ec", low="#d3c1f6") +
  theme_light() +
  labs(y="Cantidad de nacidos vivos", x="Año") +
  theme(text=element_text(family="font_sans"),
        legend.position="none",
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font", face="italic"),
        panel.grid.major = element_line(color="grey95", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(family="font_sans", size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(family="font_sans", size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font_sans", lineheight = 1),
        axis.title.y = element_text(size=15, family="font_sans"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=10, height=5)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=10, height=5)

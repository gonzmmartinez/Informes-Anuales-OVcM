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
  rename(Rango_etario = "Edad") %>%
  group_by(Año, Provincia, Rango_etario) %>%
  summarise(Cantidad = sum(Cantidad))

Totales_pais <- Nacidos_vivos %>%
  group_by(Año, Rango_etario) %>%
  summarise(Cantidad = sum(Cantidad), .groups = "drop") %>%
  mutate(Provincia = "Total del país")

Nacidos_vivos <- Nacidos_vivos %>%
  rbind(Totales_pais)

# LEER DATOS DE POBLACIÓN
# Rango de edades (etiquetas quinquenales, para adjuntar a los datos)
edades <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
            "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
            "80-84","85-89","90-94","95-99","100+")

Diccionario_edades <- data.frame(Edad = edades,
                                 Rango_etario = c(NA, NA, "Menos de 15 años",
                                                  "15-19 años", "20-24 años", "25-29 años", "30-34 años",
                                                  "35-39 años", "40-44 años", "45 años o más", "45 años o más",
                                                  "45 años o más", "45 años o más", "45 años o más", "45 años o más",
                                                  "45 años o más", "45 años o más", "45 años o más", "45 años o más",
                                                  "45 años o más", "45 años o más"))

# Definimos en qué rango de celdas está cada año
rangos <- list(
  "2010" = "D8:D28",
  "2011" = "H8:H28",
  "2012" = "L8:L28",
  "2013" = "P8:P28",
  "2014" = "T8:T28",
  "2015" = "X8:X28",
  "2016" = "D36:D56",
  "2017" = "H36:H56",
  "2018" = "L36:L56",
  "2019" = "P36:P56",
  "2020" = "T36:T56",
  "2021" = "X36:X56",
  "2022" = "D64:D84",
  "2023" = "H64:H84"
)

# Nombres de todas las hojas (provincias)
archivo <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/Datos/Proyecciones_poblacion_provincias.xls")
hojas <- excel_sheets(archivo)[-1]

# Función para leer una hoja completa
leer_hoja <- function(hoja) {
  map_dfr(names(rangos), function(anio) {
    datos <- read_excel(archivo, sheet = hoja, range = rangos[[anio]], col_names = FALSE)[[1]]
    tibble(
      provincia = hoja,
      anio = as.integer(anio),
      edad = edades,
      poblacion = as.numeric(datos)
    )
  })
}

# Aplicamos a todas las hojas
Poblacion_raw <- map_dfr(hojas, leer_hoja)

Poblacion <- Poblacion_raw %>%
  rename(Provincia = "provincia", Año = "anio", Edad = "edad", Poblacion = "poblacion") %>%
  left_join(Diccionario_edades, by="Edad") %>%
  select(Año, Provincia, Rango_etario, Poblacion) %>%
  group_by(Año, Provincia, Rango_etario) %>%
  summarise(Poblacion = sum(Poblacion)) %>%
  ungroup %>%
  mutate(Provincia = str_sub(Provincia, start=1, end=2)) %>%
  rename(Codigo = "Provincia") %>%
  left_join(Codigos_provincias, by="Codigo") %>%
  select(Año, Provincia, Rango_etario, Poblacion)

Poblacion_completa <- Poblacion %>%
  group_by(Provincia, Rango_etario) %>%
  group_modify(~ {
    modelo <- lm(Poblacion ~ poly(Año, 2), data = .x)
    
    nuevos <- tibble(
      Año = 2005:2009,
      Poblacion = round(predict(modelo, newdata = data.frame(Año = 2005:2009)))
    )
    
    bind_rows(nuevos, .x)
  }) %>%
  ungroup() %>%
  arrange(Provincia, Rango_etario, Año) #%>%
  #filter(Provincia %in% c("Salta", "Total del país"))


# UNIÓN DE TODOS LOS DATOS
Data <- Nacidos_vivos %>%
  filter(Provincia %in% c("Salta", "Total del país")) %>%
  rename(Nacimientos = "Cantidad") %>%
  left_join(Poblacion_completa, 
            by = c("Año", "Provincia", "Rango_etario")) %>%
  mutate(Tasa = (1000 * Nacimientos) / Poblacion) %>%
  arrange(Provincia, Año) %>%
  mutate(Rango_etario = factor(Rango_etario,
                               levels = c("Menos de 15 años", "15-19 años",
                                          "20-24 años", "25-29 años", "30-34 años",
                                          "35-39 años", "40-44 años", "45 años o más"))) %>%
  filter(Rango_etario %in% c("Menos de 15 años", "15-19 años"))

Data_totales <- Data %>%
  group_by(Año, Provincia) %>%
  summarise(Poblacion = sum(Poblacion),
            Nacimientos = sum(Nacimientos)) %>%
  ungroup %>%
  mutate(Tasa = (1000 * Nacimientos) / Poblacion)

Data <- Data %>%
  rbind(Data_totales %>% mutate(Rango_etario="Adolescencia")) %>%
  mutate(Rango_etario = case_when(Rango_etario == "Menos de 15 años" ~ "TEF adolescente temprana (10-14 años)",
                                  Rango_etario == "15-19 años" ~ "TEF adolescente tardía (15-19 años)",
                                  Rango_etario == "Adolescencia" ~ "TEF adolescente total (10-19 años)"),
         Provincia = ifelse(Provincia == "Salta", "Salta", "Argentina")) %>%
  mutate(Rango_etario = factor(Rango_etario,
                               levels=c("TEF adolescente temprana (10-14 años)", "TEF adolescente tardía (15-19 años)",
                                        "TEF adolescente total (10-19 años)")),
         Provincia = factor(Provincia, levels=c("Salta", "Argentina")))

################################################################################
# Datos de provincias de ENIA
Data2 <- Nacidos_vivos %>%
  filter(Provincia %in% c("Salta", "Jujuy", "Tucumán", "Formosa", "Chaco", "Catamarca",
                          "La Rioja", "Santiago del Estero", "Misiones", "Corrientes",
                          "Entre Ríos", "Buenos Aires", "Total del país")) %>%
  rename(Nacimientos = "Cantidad") %>%
  left_join(Poblacion_completa, 
            by = c("Año", "Provincia", "Rango_etario")) %>%
  mutate(Tasa = (1000 * Nacimientos) / Poblacion) %>%
  arrange(Provincia, Año) %>%
  mutate(Rango_etario = factor(Rango_etario,
                               levels = c("Menos de 15 años", "15-19 años",
                                          "20-24 años", "25-29 años", "30-34 años",
                                          "35-39 años", "40-44 años", "45 años o más"))) %>%
  filter(Rango_etario %in% c("Menos de 15 años", "15-19 años"))

Data_totales2 <- Data2 %>%
  group_by(Año, Provincia) %>%
  summarise(Poblacion = sum(Poblacion),
            Nacimientos = sum(Nacimientos)) %>%
  ungroup %>%
  mutate(Tasa = (1000 * Nacimientos) / Poblacion)

Data2 <- Data2 %>%
  rbind(Data_totales2 %>% mutate(Rango_etario="Adolescencia")) %>%
  mutate(Rango_etario = case_when(Rango_etario == "Menos de 15 años" ~ "TEF adolescente temprana (10-14 años)",
                                  Rango_etario == "15-19 años" ~ "TEF adolescente tardía (15-19 años)",
                                  Rango_etario == "Adolescencia" ~ "TEF adolescente total (10-19 años)")) %>%
  mutate(Rango_etario = factor(Rango_etario,
                               levels=c("TEF adolescente temprana (10-14 años)", "TEF adolescente tardía (15-19 años)",
                                        "TEF adolescente total (10-19 años)"))) %>%
  filter(Año %in% c(2017, 2023),
         Rango_etario == "TEF adolescente total (10-19 años)")
################################################################################

# Colores
Paleta <- c("#206170", "#5ec5d4", "#a782ec", "#852f8c", "#0f216d", "#2b42a0",
            "#ff9d27", "#ff621d", "#f93e35", "#d3335e", "#cbc2ce")

Colores <- c("TEF adolescente temprana (10-14 años)" = "#5ec5d4",
             "TEF adolescente tardía (15-19 años)" = "#d3335e",
             "TEF adolescente total (10-19 años)" = "#2b42a0")

# Grafico
grafico <- ggplot(Data, aes(x=Año, y=Tasa)) +
  geom_vline(xintercept=2017, linewidth=0.5, linetype=2, color="grey25") +
  annotate("text", x = 2017 + 0.5, y = 85,
           label = str_wrap("Implementación del Plan ENIA", width = 15),
           size = 4, color = "grey25", hjust = 0,
           family = "font_sans", fontface = "italic", lineheight = 0.75) +
  geom_line(aes(color=Rango_etario, group=Rango_etario), lineend = "round", linewidth=1) +
  facet_wrap(~Provincia, nrow=1) +
  scale_x_continuous(breaks = seq(from=2005, to=2025, by=5),
                     labels = function(z) formatC(z, big.mark=".", decimal.mark=",", format="fg"),
                     limits=c(2005, 2025)) +
  scale_y_continuous(labels = function(z) formatC(z, format = "f", digits = 1, big.mark = ".", decimal.mark = ","),
                     expand = c(0,0), breaks = seq(from=0, to=100, by=25), limits=c(-3, 103)) +
  scale_color_manual(values=Colores, labels=function(z) str_wrap(z, width=25)) +
  theme_light() +
  labs(y="Tasa de fecundidad adolescente\n(hijos/as por 1.000 mujeres)") +
  theme(text=element_text(family="font_sans"),
        legend.position="bottom",
        legend.justification = "center",
        legend.title = element_blank(),
        legend.text = element_text(size=12, family="font_sans"),
        legend.key.spacing.x = unit(1, "cm"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption = element_text(size=12, family="font_sans", face="italic"),
        panel.grid.major = element_line(color="grey95", linewidth = 0.5),
        panel.grid = element_blank(),
        axis.text.x = element_text(size=12, margin = margin(t=10,r=0,b=5,l=0)),
        axis.text.y = element_text(size=12, margin = margin(t=0,r=10,b=0,l=5)),
        axis.title.x = element_text(size=15, family="font_sans", lineheight = 1),
        axis.title.y = element_text(size=15, family="font_sans"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background = element_rect(color=NA, fill="#cbc2ce"),
        strip.text = element_text(size=15, color="black", family="font_serif", face="bold",
                                  margin=margin(t=10, b=10)))

# Guardar gráfico
filename <- str_sub(basename(rstudioapi::getSourceEditorContext()$path), 1,
                    str_length(unlist(basename(rstudioapi::getSourceEditorContext()$path)))-2)

ggsave(filename = paste0(filename, ".png"),
       path = paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PNG/"),
       plot=grafico, dpi=100, width=12, height=6)
ggsave(filename = paste0(filename, ".pdf"),
       path=paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Graficos/PDF/"),
       plot=grafico, dpi=72, width=12, height=6)
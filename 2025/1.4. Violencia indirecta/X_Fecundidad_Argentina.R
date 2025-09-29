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

# Fuentes
library(showtext)
font_add_google("Roboto", "font")
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
                                 Rango_etario = c("Menos de 15 años", "Menos de 15 años", "Menos de 15 años",
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
  arrange(Provincia, Rango_etario, Año)


# UNIÓN DE TODOS LOS DATOS
Data <- Nacidos_vivos %>%
  rename(Nacimientos = "Cantidad") %>%
  filter(Rango_etario %in% c("15-19 años", "20-24 años", "25-29 años", 
                             "30-34 años", "35-39 años", "40-44 años", "45 años o más")) %>%
  left_join(Poblacion_completa, 
            by = c("Año", "Provincia", "Rango_etario")) %>%
  mutate(Tasa = Nacimientos / Poblacion) %>%
  group_by(Provincia, Año) %>%
  summarise(TGF = sum(Tasa * 5), .groups = "drop") %>%
  arrange(Provincia, Año)


# Colores
Colores <- c("Menos de 15 años" = "#d72b31",
             "15-19 años" = "#e94a22",
             "20-24 años" = "#f69e31",
             "25-29 años" = "#e8fa42",
             "30-34 años" = "#60c04c",
             "35-39 años" = "#21917b",
             "40-44 años" = "#225575",
             "45 años o más" = "#5f3675",
             "Sin especificar" = "#6c757d")

# Grafico
grafico <- ggplot(Data %>% filter(Provincia %in% c("Salta", "Total del país")),
                  aes(x=Año, y=TGF)) +
  geom_line(aes(color=Provincia)) +
  geom_text(aes(label=round(TGF, 1)), nudge_y=0.05) +
  theme_light() +
  theme(text = element_text(size=20, family="font"),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, family="font"),
        axis.text.y = element_text(size=10, family="font"),
        axis.title = element_text(size=12, family="font"),
        plot.title = element_text(family="font", face="bold"),
        plot.subtitle = element_text(size=15, family="font", face="italic"),
        plot.caption = element_text(size=12, family="font"),
        plot.caption.position = "plot",
        panel.grid.minor.x = element_blank())

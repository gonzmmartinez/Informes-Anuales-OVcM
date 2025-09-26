# Limpiar todo
rm(list = ls())

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
  mutate(Provincia = ifelse(Provincia == "Ciudad Aut. de Buenos Aires", "Ciudad Autónoma de Buenos Aires", Provincia))

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
  select(Año, Provincia, Edad, Cantidad)

# LEER DATOS DE POBLACIÓN
# Rango de edades (etiquetas quinquenales, para adjuntar a los datos)
edades <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
            "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",
            "80-84","85-89","90-94","95-99","100+")

Diccionario_edades <- data.frame(Codigo_edad = edades,
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
Poblacion <- map_dfr(hojas, leer_hoja) %>%
  rename(Provincia = "provincia", Año = "anio", Edad = "edad", Poblacion = "poblacion") %>%
  mutate(Edad = case_when())

Poblacion <- data.frame(Año=2010:2023,
                        Mujeres=c(128976,129724,129933,129718,129219,128571,
                                  127877,127204,126611,126114,125820,126148,
                                  126579,127113))
modelo2 <- lm(Mujeres ~ poly(Año, 2), data = Poblacion)
predicciones2 <- predict(modelo2, newdata = data.frame(Año = 2005:2009))
Poblacion_completa <- bind_rows(
  data.frame(Año = 2005:2009, Mujeres = round(predicciones2)),
  Poblacion
)



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
grafico <- ggplot(Data_Salta, aes(x=Año)) +
  geom_col(aes(y=Cantidad/100), fill="#bababa", alpha=0.75) +
  geom_line(aes(y=Tasa), linewidth=3, color="#e94a22") +
  geom_point(aes(y=Tasa), size=5, color="#e94a22") +
  labs(title="Tasa de fecundidad adolescente",
       subtitle="Cantidad de nacidos vivos de madres de menos de 19 años. Provincia de Salta. Periodo 2005-2023",
       caption="FUENTE: Ministerio de Salud. Dirección de Estadística e Información en Salud (DEIS).\nNOTA: La tasa de fecundidad adolescente se calcula como la cantidad de nacidos vivos por cada 1000 mujeres de entre 10 y 19 años.",
       x="Año", y="Tasa de fecundidad adolescente") +
  scale_x_continuous(breaks=2005:2023) +
  scale_y_continuous(sec.axis = sec_axis(transform=~.*100, name="Cantidad de nacidos vivos de madres de menos de 19 años")) +
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

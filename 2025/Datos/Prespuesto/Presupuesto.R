# Limpiar todo
rm(list = ls())

# Librerías
library(dplyr)
library(purrr)
library(readr)

# Carpeta donde están los archivos
carpeta <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Generar vector de archivos (2014 a 2024)
archivos <- paste0(carpeta, "/ejecucion-fisica-trimestre-4-", 2014:2024, ".csv")

# Leer todos y unir en un solo dataframe
Raw <- map_dfr(archivos, ~ read_csv(.x, col_types = cols(.default = col_character())))

Data <- Raw %>%
  filter(funcion_desc == "Salud") %>%
  filter(medicion_fisica_desc %in% c("Distribución de Medicamentos para la Interrupción Voluntaria del Embarazo (Ley N° 27.610)",
                                     "Distribución de Métodos Anticonceptivos de Larga Duración para Adolescentes (Plan Enia)",
                                     "Distribución de Preservativos",
                                     "Asistencia en Salud Sexual y Reproductiva",
                                     "Asistencia en Salud Sexual y Reproductiva (PPG)")) %>%
  select(Año = ejercicio_presupuestario,
         Categoria = medicion_fisica_desc,
         Vigente = programacion_anual_vig_trim4,
         Ejecutado = ejecutado_acumulado_trim4) %>%
  mutate(Categoria = ifelse(Categoria == "Asistencia en Salud Sexual y Reproductiva (PPG)", "Asistencia en Salud Sexual y Reproductiva", Categoria)) %>%
  group_by(Año, Categoria) %>%
  summarise(Ejecutado = sum(as.numeric(Ejecutado)),
            Vigente = sum(as.numeric(Vigente))) %>%
  ungroup %>%
  arrange(Categoria, Año)

# Guardar csv
write_csv(Data, file.path(carpeta, "Presupuesto.csv"))

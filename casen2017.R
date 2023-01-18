library(crosstable)
library(dplyr)
library(gt)
library(survey)
library(kableExtra)
library(openxlsx)

# Valores en libro de códigos Casen
areas_ciencia <- c(
  50, # Cs. Naturales, Matemáticas y Estadísticas sin mayor definición
  51, # Cs. Biológicas y Afines
  52, # Medio Ambiente
  53, # Ciencias Físicas
  54, # Matemáticas y Estadísticas
  61, # TIC
  71, # Ingeniería y Profesiones Afines
  72, # Industria y Producción
  73, # Arquitectura y Construcción
  81, # Agricultura
  82  # Silvicultura
)

educacion_tecnica <- c(
  12, # Técnica Incompleta
  13  # Técnica completa
)
educacion_profesional <- c(
  14, # Profesional Incompleta
  15, # Profesional Completa
  16, # Postgrado Incompleta
  17  # Postgrado Completa
)

ciencia_areas_rows <- c(6:9)
ciencia_subareas_rows <- c(12:17, 19:21, 23:24)

table_styles <- function(table) {
  table %>% addmargins() %>% kbl() %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}

prepare_profesionales_ciencia <- function(data) {
  new_data <- data %>%
    mutate(profesional_ciencia = case_when(data$e7_subarea %in% areas_ciencia ~ TRUE)) %>%
    mutate(nivel_educacional = case_when(data$e6a %in% educacion_tecnica ~ 'Técnico',
                                         data$e6a %in% educacion_profesional ~ 'Profesional'))

  subset(new_data, profesional_ciencia == TRUE)
}

add_cv_variables <- function(data) {
  # Variables de SPSS que tienen valor y etiqueta para usar más fácilmente
  area_ocupacion_labels <- haven::as_factor(data$e7_cod_area, levels = "label")
  subarea_ocupacion_labels <- haven::as_factor(data$e7_subarea, levels = "label")
  region_labels <- haven::as_factor(data$region, levels = "label")

  data$rango_salario <- cut(data$y1, c(seq(0, 10000000, 500000), Inf), labels = c(
    "< 500000",
    "500000-1000000",
    "1000000-1500000",
    "1500000-2000000",
    "2000000-2500000",
    "2500000-3000000",
    "3000000-3500000",
    "3500000-4000000",
    "4000000-4500000",
    "4500000-5000000",
    "5000000-5500000",
    "5500000-6000000",
    "6000000-6500000",
    "6500000-7000000",
    "7000000-7500000",
    "7500000-8000000",
    "8000000-8500000",
    "8500000-9000000",
    "9000000-9500000",
    "9500000-10000000",
    "> 10000000"
  ))
  data %>% mutate(ocupacion = area_ocupacion_labels, subarea_ocupacion = subarea_ocupacion_labels, region_name = region_labels)
}

build_crosstables <- function(data_ciencia) {
  dsn <- svydesign(id = ~ data_ciencia$folio, weights = ~ data_ciencia$expr, data = data_ciencia)

  # Generar tablas de frecuencia
  freq_area_region <- svytable(~ data_ciencia$ocupacion + data_ciencia$region_name, dsn)
  freq_subarea_region <- svytable(~ data_ciencia$subarea_ocupacion + data_ciencia$region_name, dsn)
  freq_area_salario <- svytable(~ data_ciencia$ocupacion + data_ciencia$rango_salario, dsn)
  freq_subarea_salario <- svytable(~ data_ciencia$subarea_ocupacion + data_ciencia$rango_salario, dsn)
  freq_region_salario <- svytable(~ data_ciencia$region_name + data_ciencia$rango_salario, dsn)

  kable_area_region <- freq_area_region[ciencia_areas_rows,] %>% table_styles()
  kable_subarea_region <- freq_subarea_region[ciencia_subareas_rows,] %>% table_styles()
  kable_area_salario <- freq_area_salario[ciencia_areas_rows,] %>% table_styles()
  kable_subarea_salario <- freq_subarea_salario[ciencia_subareas_rows,] %>% table_styles()
  kable_region_salario <- freq_region_salario %>% table_styles()

  print(kable_area_region)
  print(kable_subarea_region)
  print(kable_area_salario)
  print(kable_subarea_salario)
  print(kable_region_salario)
}

# 1. Leer archivo
data <- haven::read_sav("Casen 2017.sav")

# 2. Agregar variables necesarias
data_ciencia <- prepare_profesionales_ciencia(data)
data_ciencia <- add_cv_variables(data_ciencia)

write.csv(data_ciencia, './data_ciencia.csv')
write.xlsx(data_ciencia, './data_ciencia.xlsx')

# 3. Generar tablas de frequencia
build_crosstables(data_ciencia)


library(crosstable)
library(dplyr)
library(gt)
library(survey)
library(kableExtra)

prepare_profesionales_ciencia <- function(data) {
  # Valores en libro de códigos Casen
  areas_ciencia <- c(50, 51, 52, 53, 54, 61, 71)

  new_data <- data %>%
    mutate(profesional_ciencia = case_when(data$e7_subarea %in% areas_ciencia ~ TRUE))

  subset(new_data, profesional_ciencia == TRUE)
}

add_cv_variables <- function(data) {
  # Variables de SPSS que tienen valor y etiqueta para usar más fácilmente
  area_ocupacion_labels <- haven::as_factor(data$e7_cod_area, levels = "label")
  subarea_ocupacion_labels <- haven::as_factor(data$e7_subarea, levels = "label")
  region_labels <- haven::as_factor(data$region, levels = "label")

  data %>% mutate(ocupacion = area_ocupacion_labels, subarea_ocupacion = subarea_ocupacion_labels, region_name = region_labels)
}

run_analyses <- function(data_ciencia) {
  dsn <- svydesign(id = ~ data_ciencia$folio, weights = ~ data_ciencia$expr, data = data_ciencia)

  # Generar tablas de frecuencia
  freq_area_region <- svytable(~ data_ciencia$ocupacion + data_ciencia$region_name, dsn)
  freq_subarea_region <- svytable(~ data_ciencia$subarea_ocupacion + data_ciencia$region_name, dsn)

  kable_area_region <- freq_area_region[6:8, ] %>%
    addmargins() %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  kable_subarea_region <- freq_subarea_region[c(12:17, 19), ] %>%
    addmargins() %>%
    kbl() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

  print(kable_area_region)
  print(kable_subarea_region)
}

# 1. Leer archivo
data <- haven::read_sav("Casen 2017.sav")

# 2. Agregar variables necesarias
data_ciencia <- prepare_profesionales_ciencia(data)
data_ciencia <- add_cv_variables(data_ciencia)

# 3. Generar tablas de frequencia
run_analyses(data_ciencia)

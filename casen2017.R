library(survey)
library(crosstable)
library(dplyr)
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

extract_profesionales_ciencia <- function(data) {
  new_data <- data %>%
    mutate(profesional_ciencia = case_when(data$e7_subarea %in% areas_ciencia ~ TRUE)) %>%
    mutate(nivel_educacional = case_when(data$e6a %in% educacion_tecnica ~ 'Técnico',
                                         data$e6a %in% educacion_profesional ~ 'Profesional'))

  subset(new_data, profesional_ciencia == TRUE)
}

add_cv_variables <- function(data) {
  # Variables de SPSS que tienen etiqueta además de valor para usar más fácilmente
  # el nombre y no el código numérico.
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

  data %>% mutate(
    area_ocupacion = area_ocupacion_labels,
    subarea_ocupacion = subarea_ocupacion_labels,
    region_nombre = region_labels)
}

table_styles <- function(table) {
  table %>%
    addmargins() %>%
    kbl() %>% # Wrapper function of knitr::kable
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
}

export <- function(table, filename) {
  table %>% cat(., file  = paste0("./tables/", filename))
}

build_crosstables <- function(data_ciencia, crosstables) {
  dsn <- svydesign(id = ~ data_ciencia$folio, weights = ~ data_ciencia$expr, data = data_ciencia)

  for (crosstable in crosstables) {
    frequency_table <- svytable(crosstable$formula, dsn)

    if (is.null(crosstable$rows)) {
      kable_table <- frequency_table %>% table_styles()
    } else {
      kable_table <- frequency_table[crosstable$rows,] %>% table_styles()
    }

    kable_table %>% export(crosstable$filename)
    print(kable_table)
  }
}

# 1. Leer archivo
data <- haven::read_sav("Casen 2017.sav")

# 2. Agregar variables necesarias
data_ciencia <- extract_profesionales_ciencia(data)
data_ciencia <- add_cv_variables(data_ciencia)

crosstables <- list(
  list(formula = ~ data_ciencia$area_ocupacion + data_ciencia$region_nombre, rows = ciencia_areas_rows, filename = "area_region.html"),
  list(formula = ~ data_ciencia$subarea_ocupacion + data_ciencia$region_nombre, rows = ciencia_subareas_rows, filename = "subarea_region.html"),
  list(formula = ~ data_ciencia$area_ocupacion + data_ciencia$rango_salario, rows = ciencia_areas_rows, filename = "area_salario.html"),
  list(formula = ~ data_ciencia$subarea_ocupacion + data_ciencia$rango_salario, rows = ciencia_subareas_rows, filename = "subarea_salario.html"),
  list(formula = ~ data_ciencia$region_nombre + data_ciencia$rango_salario, rows = NULL, filename = "region_salario.html"),
  list(formula = ~ data_ciencia$nivel_educacional + data_ciencia$region_nombre, rows = NULL, filename = "nivel_educacional_region.html"),
  list(formula = ~ data_ciencia$nivel_educacional + data_ciencia$rango_salario, rows = NULL, filename = "nivel_educacional_salario.html")
)

# 3. Generar tablas de frequencia
build_crosstables(data_ciencia, crosstables)

# 4. Exportar xlsx y csv
write.csv(data_ciencia, './data_ciencia.csv')
write.xlsx(data_ciencia, './data_ciencia.xlsx')

library(survey)
library(crosstable)
library(dplyr)
library(kableExtra)
library(openxlsx)
library(ggplot2)
library(ggeasy)

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

sector_publico <- c(
  3, # Empleado u obrero sector público (gob. central o municipal)
  4, # Empleado u obrero empresas públicas
  8  # FFAA
)

sector_privado <- c(
  1, # Patrón o empleador
  2, # Trabajador por cuenta propia
  5, # Empleado u obrero sector privado
  6, # Servicio doméstico puertas adentro
  7, # Servicio doméstico puertas afuera
  9  # Familiar no remunerado
)

ciencia_areas_rows <- c(6:9)
ciencia_subareas_rows <- c(12:17, 19:21, 23:24)

add_cv_variables <- function(data) {
  # Variables de SPSS que tienen valor y además etiqueta. Se extrae la etiqueta para usar más
  # fácilmente el nombre y no el código numérico.
  area_ocupacion_labels <- haven::as_factor(data$e7_cod_area, levels = "label")
  subarea_ocupacion_labels <- haven::as_factor(data$e7_subarea, levels = "label")
  region_labels <- haven::as_factor(data$region, levels = "label")
  industria_labels <- haven::as_factor(data$rama1, levels = "label")
  tipo_jornada_labels <- haven::as_factor(data$o18, levels = "label")

  data$salario <- data$y1
  data$salario[data$salario == 99] <- NA # 99 = No sabe
  data$salario[data$salario == 0] <- NA

  # TODO: borrar, ya no es necesario
  data$rango_salario <- cut(data$salario, c(seq(0, 10000000, 500000), Inf), labels = c(
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

  data %>%
    mutate(
      area_ocupacion = area_ocupacion_labels,
      subarea_ocupacion = subarea_ocupacion_labels,
      region_nombre = region_labels,
      industria = industria_labels
    ) %>%
    mutate(nivel_educacional = case_when(data$e6a %in% educacion_tecnica ~ 'Técnico',
                                         data$e6a %in% educacion_profesional ~ 'Profesional')) %>%
    mutate(sector = case_when(data$o15 %in% sector_publico ~ 'Público',
                              data$o15 %in% sector_privado ~ 'Privado')) %>%
    mutate(profesional_ciencia = case_when(data$e7_subarea %in% areas_ciencia ~ TRUE)) %>%
    mutate(participa_sindicato = case_when(data$o24a == 1 ~ TRUE)) %>%
    mutate(participa_asociacion_funcionarios = case_when(data$o24b == 1 ~ TRUE)) %>%
    mutate(participa_asociacion_gremial = case_when(data$o24c == 1 ~ TRUE)) %>%
    mutate(participa_colegio_profesional = case_when(data$o24d == 1 ~ TRUE)) %>%
    mutate(tipo_jornada = case_when(data$o18 == 1 ~ "Completa",
                                    data$o18 == 2 ~ "Parcial",
                                    data$o18 == 3 ~ "Prolongada",
                                    data$o18 == 4 ~ "Otra",
                                    data$o18 == 9 ~ "NS/NR")) %>%
    mutate(tipo_contrato = case_when(data$o16 == 1 ~ "Contrato indefinido",
                                     data$o16 == 2 ~ "Plazo fijo",
                                     data$o16 == 9 ~ "NS/NR"))

}

extract_profesionales_ciencia <- function(data) {
  subset(data, data$profesional_ciencia == TRUE)
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
  folio <- data_ciencia$folio
  factor_expansion_region <- data_ciencia$expr

  dsn <- svydesign(id = ~ folio, weights = ~ expr, data = data_ciencia)

  for (crosstable in crosstables) {
    formula <- crosstable$formula
    rows <- crosstable$rows
    filename <- crosstable$filename

    frequency_table <- svytable(formula, dsn)

    if (is.null(rows)) {
      kable_table <- frequency_table %>% table_styles()
    } else {
      kable_table <- frequency_table[rows,] %>% table_styles()
    }

    kable_table %>% export(filename)
    print(kable_table)
  }
}

# 1. Leer archivo
data <- haven::read_sav("Casen 2017.sav")

# 2. Agregar variables necesarias
data_ciencia <- add_cv_variables(data)

# TODO: borrar
ggplot(data_ciencia, aes(x = area_ocupacion, weight = expr, color = area_ocupacion)) +
  geom_histogram()

data_ciencia <- extract_profesionales_ciencia(data_ciencia)

crosstables <- list(
  list(formula = ~ data_ciencia$area_ocupacion + data_ciencia$region_nombre, rows = ciencia_areas_rows, filename = "area_region.html"),
  list(formula = ~ data_ciencia$subarea_ocupacion + data_ciencia$region_nombre, rows = ciencia_subareas_rows, filename = "subarea_region.html"),
  list(formula = ~ data_ciencia$nivel_educacional + data_ciencia$region_nombre, rows = NULL, filename = "nivel_educacional_region.html"),
  list(formula = ~ data_ciencia$industria + data_ciencia$region_nombre, rows = NULL, filename = "industria_region.html"),
  list(formula = ~ data_ciencia$area_ocupacion + data_ciencia$sector, rows = ciencia_areas_rows, filename = "area_sector.html"),
  list(formula = ~ data_ciencia$subarea_ocupacion + data_ciencia$tipo_contrato, rows = ciencia_subareas_rows, filename = "subarea_tipo_contrato.html")
)

crosstables_total <- list(
  list(formula = ~ data_ciencia$area_ocupacion + data_ciencia$region_nombre, rows = NULL, filename = "area_region_total.html")
)

# 3. Generar tablas de frequencia
build_crosstables(data_ciencia, crosstables)
build_crosstables(data, crosstables_total)

# data_ciencia_cleaned <- data_ciencia %>% filter(!is.na(salario))
#
# salarios_por_region <- data_ciencia_cleaned %>% group_by(region_nombre) %>% summarize(salarios = list(salario))
# salarios_por_area <- data_ciencia_cleaned %>% group_by(area_ocupacion) %>% summarize(salarios = list(salario))
# salarios_por_subarea <- data_ciencia_cleaned %>% group_by(subarea_ocupacion) %>% summarize(salarios = list(salario))
# salarios_por_nivel_educacional <- data_ciencia_cleaned %>% group_by(nivel_educacional) %>% summarize(salarios = list(salario))
# salarios_por_industria <- data_ciencia_cleaned %>% group_by(industria) %>% summarize(salarios = list(salario))
# salarios_por_sector <- data_ciencia_cleaned %>% group_by(sector) %>% summarize(salarios = list(salario))

# TODO: extraer a una función similar a build_crosstables
ggplot(data_ciencia, aes(x = subarea_ocupacion, y = salario, weight = expr, fill = subarea_ocupacion)) +
  geom_boxplot() +
  scale_shape_manual(name = "", labels = c("Mediana", "Promedio"), values = c(16, 23)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3) +
  scale_y_log10(labels = unit_format(unit = "M", scale = 1e-6)) +
  easy_rotate_x_labels(angle = 70, side = "right") +
  labs(x = "Subárea", y = "Salario", subtitle = "Distribución Subárea / Salario")

# 4. Exportar xlsx y csv
write.csv(data_ciencia, './data_ciencia.csv')
write.xlsx(data_ciencia, './data_ciencia.xlsx')

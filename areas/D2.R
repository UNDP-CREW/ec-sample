#================================================================
# Área: Cohesión social, equidad y no discriminación
#================================================================

# librerías
library(readxl)
library(tidyverse)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#================== 2001 / 2002 / 2003 ==========================
# Pobreza
# Pobreza por ingresos / Pobreza extrema por ingresos / Índice de Gini
# Estos indicadores necesitan cinco argumentos:
#   archivo (str) = ubicación y nombre de archivo
#   codigo_indicador (int) = código de indicador a actualizar
#   mes (int) = número de mes para la actualización
#   año (int) = año
#   guardar (boolean) = guardar archivo actualizado


act_pobreza <- function(archivo, codigo_indicador, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Hojas de trabajo de acuerdo a código de indicador
  if (codigo_indicador == 2001){
    area_indicador <- c("1.1.1.pobre_nacional", "1.1.2.pobre_urbana", "1.1.3.pobre_rural")
    columna_datos = 'Incidencia (1)'
  } else if (codigo_indicador == 2002) {
    area_indicador <- c("1.2.1.extpob_nacional", "1.2.2.extpob_urbana", "1.2.3.extpob_rural")
    columna_datos = 'Incidencia (1)'
  } else if (codigo_indicador == 2003) {
    area_indicador <- c("2.1. Desigualdad_nacional ", "2.2. Desigualdad_urbana", "2.3. Desigualdad_rural ")
    columna_datos = 'Índice de Gini'
  }
  
  
  # Crear 'loop' por cada hoja del archivo xlsx, según código de indicador
  for (area in area_indicador){
    
    archivo_inicial <- read_excel(paste0("source data/", archivo),
                                  sheet = area,
                                  skip = 8)
    
    # Extraer el dato numérico de cada hoja
    nuevo_dato <- archivo_inicial %>%
      rename('Mes' = 'Período',
             'Año' = '...2',
             'Dato Numérico' = all_of(columna_datos)) %>%
      filter(!is.na(Año)) %>%
      mutate(Área = str_replace_all(str_extract(area, "(?<=\\_)[a-z].*"), ' ', ''),
             `Dato Numérico` = as.numeric(str_replace_all(`Dato Numérico`, '-', '')),
             Año = as.numeric(str_replace_all(Año, '\\s\\([\\d]\\)', '')) ,
             Mes = case_when(
               Mes == 'Junio' ~ 6,
               Mes == 'Diciembre' ~ 12)) %>%
      fill('Mes', .direction = 'down') %>%
      select(Mes, Año, Área, `Dato Numérico`)
    
    # Ordenar los datos extraídos. El primer dato corresponde al 'Total Nacional'. Las filas subsiguientes se unen a esta.
    if (area  == area_indicador[1]){
      nuevo_dato_semestre <- nuevo_dato
    } else {
      nuevo_dato_semestre <- rbind(nuevo_dato_semestre, nuevo_dato) %>%
        arrange(desc(Año), desc(Mes))
    }
  }
  
  
  # Recolectar dato por cada semestre
  nuevo_dato_semestre <- nuevo_dato_semestre %>%
    mutate(Área = case_when(
      Área == 'nacional' ~ 'Total',
      Área == 'urbana' ~ 'Urbano',
      Área == 'rural' ~ 'Rural')) %>%
    filter(Año == año & Mes == mes)
  
  
  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato_semestre, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#================== 2004 / 2005 =================================
# Pobreza multidimensional
# Pobreza multidimensional / Pobreza extrema multidimensional
# Estos indicadores necesitan cuatro argumentos:
#   archivo (str) = ubicación y nombre de archivo
#   codigo_indicador (int) = código de indicador a actualizar
#   año (int) = año
#   guardar (boolean) = guardar archivo actualizado


act_pobreza_multi <- function(archivo, codigo_indicador, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "1.1. Serie_componentes_IPM",
                                skip = 4)
  
  
  # Extraer el dato numérico
  nuevo_dato <- archivo_inicial %>%
    rename('Área' = ...1,
           'Año' = ...2,
           Dato_2005 = `Estimación\r\npuntual...4`,
           Dato_2004 = `Estimación\r\npuntual...7`) %>%
    select('Área', 'Año', paste0('Dato_', as.character(codigo_indicador))) %>%
    filter(!is.na(Año)) %>%
    fill(Área) %>%
    mutate(Área = ifelse(Área == 'Nacional', 'Total', Área)) %>%
    rename(`Dato Numérico` = paste0('Dato_', as.character(codigo_indicador))) %>%
    filter(Año == año)
  
  
  # Datos extraídos se unen a la base importada en el primer paso y se reordena columnas
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#========================= 2018 =================================
# Protestas registradas
# Estos indicadores necesitan cuatro argumentos:
#   archivo (str) = ubicación y nombre de archivo
#   mes (int) = número de mes para la actualización
#   año (int) = año
#   guardar (boolean) = guardar archivo actualizado

act_protestas <- function(archivo, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(2018)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_csv(paste0("source data/", archivo))
  
  
  # Filtrar datos iniciales
  nuevo_dato <- archivo_inicial %>%
    filter(event_type == "Protests") %>% # se excluyen 'riots' (disturbios)
    select(event_date, sub_event_type, admin1, fatalities) %>%
    # transformar fechas
    mutate(Mes = month(dmy(event_date)),
           Año = year(dmy(event_date))) %>%
    filter(Año == año & Mes == mes)
  
  
  # Recolectar datos por desagregación geográfica (provincias)
  nuevo_dato_provincia <- nuevo_dato %>%
    count(Año, Mes, admin1) %>%
    filter(!is.na(admin1)) %>%
    rename(Provincia = admin1,
           `Dato Numérico` = n) %>%
    bind_rows(summarise(., 
                        across(`Dato Numérico`, sum),
                        across(Provincia, ~"Total Nacional"),
                        across(Año, max),
                        across(Mes, max)))
  
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia
  nuevo_dato_provincia <- cambio_nombres(nuevo_dato_provincia, 'provincia_nombre')
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_provincia, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(2018, ind, guardar)
  
  ind
  
}


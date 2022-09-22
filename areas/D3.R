#================================================================
# Área: Estabilidad económica
#================================================================

# librerías
library(readxl)
library(tidyverse)
library(readr)
library(lubridate)
source("complement/geocodigos.R", encoding = "UTF-8")
source("complement/helper.R", encoding = "UTF-8")

#================================================================

#================== 3025 / 3001 / 3002 / 3003 ===================
# Tasa de participación global, Tasa de desempleo, Tasa de empleo adecuado
# Estos indicadores necesitan dos argumentos:
#   archivo (str) = ubicación y nombre de archivo
#   codigo_indicador (str) = nombre de indicador a actualizar
#   guardar (boolean) = guardar archivo actualizado


act_enemdu_empleo <- function(archivo, codigo_indicador, guardar = FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(codigo_indicador)
  
  
  # Leer archivo con nueva información
  archivo_inicial <- read_excel(paste0("source data/", archivo),
                                sheet = "2. Tasas")
  
  
  # Extraer dato por sexo y por área
  nuevo_dato <- archivo_inicial %>%
    mutate(Indicadores_cod = case_when(
      Indicadores == 'Subempleo (%)' ~ 3025,
      Indicadores == 'Participación Global (%)' ~ 3001,
      Indicadores == 'Desempleo (%)' ~ 3002,
      Indicadores == 'Empleo Adecuado/Pleno (%)' ~ 3003
    )) %>%
    filter(Indicadores_cod == codigo_indicador) %>%
    rename(des1 = Área, # urbano
           des2 = ...6, # rural
           des3 = Sexo, # hombre
           des4 = ...8, # mujer
           des5 = Nacional) %>%
    select(Periodo, des1, des2, des3, des4, des5) %>%
    # mover base de forma horizontal a forma vertical
    pivot_longer(cols = c(des1, des2, des3, des4, des5), 
                 names_to = c("pr", "Des"),
                 names_pattern = "([a-zA-Z]+)(\\d+)",
                 values_to = "Dato Numérico") %>%
    # reemplazar valores
    mutate(`Dato Numérico` = as.numeric(`Dato Numérico`),
           Área = ifelse(Des == 1, "Urbano", ifelse(Des == 2, "Rural", "Total")),
           Sexo = ifelse(Des == 3, "Hombre", ifelse(Des == 4, "Mujer", "Total")),
           Periodo = str_replace_all(Periodo, c("-" = ".-", "sep" = "sept")),
           Fecha = parse_date(Periodo, "%b-%y", locale = locale("es"))) %>%
    mutate(Mes = month(ymd(Fecha)),
           Año = year(ymd(Fecha))) %>%
    # extraer valor más reciente
    filter(Fecha == max(Fecha)) %>%
    select(-c(Periodo, pr, Des, Fecha))
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(codigo_indicador, ind, guardar)
  
  ind
  
}

#================================================================

#========================= 3024 =================================
# Canasta familiar básica
# Este indicador necesita cuatro argumentos:
#   archivo (str) = ubicación y nombre de archivo
#   mes (int) = mes de actualización
#   año (int) = año de actualización
#   guardar (boolean) = guardar archivo actualizado


act_CFB <- function(archivo, mes, año, guardar=FALSE){
  
  # Importar base del indicador desde carpeta de área
  ind <- importar_indicador(3024)
  
  
  # Crear 'loop' por cada hoja del archivo xls
  for (location in c("1. NACIONAL", "3. CUENCA", "4. LOJA", "5. QUITO", "6. AMBATO", "8. MACHALA", "9. ESMERALDAS", "10. GUAYAQUIL", "11. MANTA", "12. STO. DOMINGO")) {
    archivo_inicial <- read_excel(paste0("source data/", archivo),
                                  sheet = location,
                                  skip = 12) # saltar filas iniciales
    
    # Extraer el dato numérico de cada hoja
    nuevo_dato <- archivo_inicial %>%
      filter(`No.\nOrden` == 1) %>% # *** 
      select(`Costo Actual en Dólares`) %>% # ***
      rename(`Dato Numérico` = `Costo Actual en Dólares`) %>%
      mutate(Mes = mes,
             Año = año,
             Cantón = case_when(
               location == "1. NACIONAL" ~ 'Total Nacional',
               location == "3. CUENCA" ~ 'Cuenca',
               location == "4. LOJA" ~ 'Loja',
               location == "5. QUITO" ~ 'Quito',
               location == "6. AMBATO" ~ 'Ambato',
               location == "8. MACHALA" ~ 'Machala',
               location == "9. ESMERALDAS" ~ 'Esmeraldas',
               location == "10. GUAYAQUIL" ~ 'Guayaquil',
               location == "11. MANTA" ~ 'Manta',
               location == "12. STO. DOMINGO" ~ 'Santo Domingo'
             ))
    
    # Ordenar los datos extraídos. El primer dato corresponde al 'Total Nacional'. Las filas subsiguientes se unen a esta.
    if (location  == "1. NACIONAL"){
      nuevo_dato_mes <- nuevo_dato
    } else {
      nuevo_dato_mes <- rbind(nuevo_dato_mes, nuevo_dato)
    }
  }
  
  
  # Unir datos extraídos con base de datos geográficos para introducir código de provincia y cantón
  nuevo_dato_mes <- cambio_nombres(nuevo_dato_mes, 'canton_nombre')
  
  
  # Datos extraídos se unen a la base importada en el primer paso
  ind <- bind_rows(nuevo_dato_mes, ind) %>%
    format_nuevo()
  
  
  # guardar indicador
  guardar_indicador(3024, ind, guardar)
  
  ind
  
}


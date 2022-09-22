library(tidyverse)
library(openxlsx)


importar_indicador <- function(codigo_indicador) {
  
  # Revisar el número de caracteres del código para identificar área de riesgo
  if (nchar(codigo_indicador) == 5){
    area = gsub('(?<=^\\d{2}).*', '', as.character(codigo_indicador), perl = TRUE)
  } else {
    area = gsub('(?<=^\\d{1}).*', '', as.character(codigo_indicador), perl = TRUE)
  }
  
  # Diccionario de áreas de riesgo
  areas_riesgo <- c('1' = 'D1_Displacement and Migration', 
                    '2' = 'D2_Social Cohesion, equality & nondiscrimination',
                    '3' = 'D3_Economic stability',
                    '4' = 'D4_Internal security',
                    '5' = 'D5_Justice and rule of law',
                    '6' = 'D6_Health',
                    '7' = 'D7_Infrastructure and access to social services',
                    '8' = 'D8_Environment and climate',
                    '9' = 'D9_Food security, agriculture & land',
                    '10' = 'D10_Gender equality')
  
  # Importar indicador
  ind <- read.csv(paste0('final data/', areas_riesgo[area], '/', as.character(codigo_indicador), '.csv'), 
                  dec = ",",
                  check.names = FALSE,
                  colClasses = c(`Código Provincia` = 'character', # mantiene ceros iniciales
                                 `Código Cantón` = 'character'))
  
}

#####

guardar_indicador <- function(codigo_indicador, ind, guardar){
  
  
  if (guardar == TRUE) {
    
    # Revisar el número de caracteres del código para identificar área de riesgo
    if (nchar(codigo_indicador) == 5){
      area = gsub('(?<=^\\d{2}).*', '', as.character(codigo_indicador), perl = TRUE)
    } else {
      area = gsub('(?<=^\\d{1}).*', '', as.character(codigo_indicador), perl = TRUE)
    }
    
    # Diccionario de áreas de riesgo
    areas_riesgo <- c('1' = 'D1_Displacement and Migration', 
                      '2' = 'D2_Social Cohesion, equality & nondiscrimination',
                      '3' = 'D3_Economic stability',
                      '4' = 'D4_Internal security',
                      '5' = 'D5_Justice and rule of law',
                      '6' = 'D6_Health',
                      '7' = 'D7_Infrastructure and access to social services',
                      '8' = 'D8_Environment and climate',
                      '9' = 'D9_Food security, agriculture & land',
                      '10' = 'D10_Gender equality')
    
    # Exportar datos
    write.table(ind %>%
                  mutate(`Dato Numérico` = as.character(`Dato Numérico`)) %>% 
                  mutate(`Dato Numérico` = stringr::str_replace(`Dato Numérico`, '\\.', ',')), # para conservar decimales con coma
                file = paste0('final data/', areas_riesgo[area], '/', as.character(codigo_indicador), '.csv'),
                sep = ',',
                qmethod = "double", 
                row.names = FALSE) 
    
  } else {
    
    
  }
  
  
}

#####

format_nuevo <- function(nuevo_dato){
  
  format_df <- nuevo_dato %>%
    fill('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 'Indicador', 
         'Área', 'Sexo', 'Población', 'Fuente', 'Unidad de medida', 'Grupo', 'Dato Cualitativo', 'ODS',
         .direction = 'up')
  
  format_df <- format_df[, c('Código Indicador', 'Código Provincia', 'Provincia', 'Código Cantón', 'Cantón', 'País', 'Dimensión', 'Subcategoría', 
                             'Indicador', 'Área', 'Sexo', 'Dato Numérico', 'Población', 'Dato Cualitativo', 'Año', 'Mes', 'Fuente', 'Unidad de medida', 
                             'Grupo', 'ODS')]
  
}

#####

exportar_xlsx <- function(area){
  
  # Diccionario de áreas de riesgo
  areas_riesgo <- c('1' = 'D1_Displacement and Migration', 
                    '2' = 'D2_Social Cohesion, equality & nondiscrimination',
                    '3' = 'D3_Economic stability',
                    '4' = 'D4_Internal security',
                    '5' = 'D5_Justice and rule of law',
                    '6' = 'D6_Health',
                    '7' = 'D7_Infrastructure and access to social services',
                    '8' = 'D8_Environment and climate',
                    '9' = 'D9_Food security, agriculture & land',
                    '10' = 'D10_Gender equality')
  
  area_dataset <- list.files(path = paste0('final data/', areas_riesgo[area], "/"), pattern = "*.csv", full.names = TRUE) %>%
    map_df(~read.csv(., dec = ",",
                     check.names = FALSE,
                     colClasses = c(`Código Provincia` = 'character',
                                    `Código Cantón` = 'character',
                                    `Código Indicador` = 'character')))
  
  write.xlsx(area_dataset, file = paste0('final data/', 'D', area, '.xlsx'), sheetName='Hoja1')
  
}

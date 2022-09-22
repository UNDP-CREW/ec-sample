library(tidyverse)


cambio_nombres <- function(data, formato){
  
  geocodigos <- read.csv("complement/geocodigos.csv",
                         sep = ";",
                         check.names = FALSE,
                         colClasses = c(CANTON_CODIGO = 'character',
                                        PROVINCIA_CODIGO = 'character'))
  # Unir por nombres
  if (formato == 'provincia_nombre'){
    
    data <- data %>%
      mutate(Provincia = str_to_title(Provincia)) %>%
      mutate(Provincia = replace(Provincia, Provincia %in% c('Santo Domingo De Los Tsachilas', 'Santo Domingo De Los Tsáchilas', 'Sto. Domingo Tsáchilas'), 'Sto Dgo Tsáchilas'),
             Provincia = replace(Provincia, Provincia %in% c('Manabi'), 'Manabí'),
             Provincia = replace(Provincia, Provincia %in% c('Los Rios'), 'Los Ríos'),
             Provincia = replace(Provincia, Provincia %in% c('Bolivar'), 'Bolívar'),
             Provincia = replace(Provincia, Provincia %in% c('Galapagos'), 'Galápagos'),
             Provincia = replace(Provincia, Provincia %in% c('Sucumbíos'), 'Sucumbios'),
             Provincia = replace(Provincia, Provincia %in% c('Nacional'), 'Total Nacional'))
    data <- data %>%
      left_join(geocodigos %>%
                  select(-c(CANTON_CODIGO, CANTON_NOMBRE)) %>%
                  unique, by = c('Provincia' = 'PROVINCIA_NOMBRE')) %>%
      mutate(`Código Provincia` = PROVINCIA_CODIGO) %>%
      select(-c(PROVINCIA_CODIGO))
    
  } else if (formato == 'canton_nombre'){
    data <- data %>%
      left_join(geocodigos, by = c('Cantón' = 'CANTON_NOMBRE')) %>%
      mutate(`Código Provincia` = PROVINCIA_CODIGO,
             Provincia = PROVINCIA_NOMBRE,
             `Código Cantón` = CANTON_CODIGO) %>%
      select(-c(CANTON_CODIGO, PROVINCIA_NOMBRE, PROVINCIA_CODIGO))
    
  } else if (formato == 'provincia_codigo'){
    data <- data %>%
      left_join(geocodigos %>%
                  select(-c(CANTON_CODIGO, CANTON_NOMBRE)) %>%
                  unique, by = c('Código Provincia' = 'PROVINCIA_CODIGO')) %>%
      rename(Provincia = PROVINCIA_NOMBRE) 
    
  }
  
}

source("areas/D2.R", encoding = "UTF-8")
source("areas/D3.R", encoding = "UTF-8")

# Cohesión social, equidad y no-discriminación

nuevo_2001 <- act_pobreza(archivo = "008_ENEMDU_Pob/202206_Tabulados_pobreza_junio_2022/202206_Tabulados_pobreza_EXCEL.xlsx", codigo_indicador = 2001, mes = 6, año = 2022, guardar = FALSE)
nuevo_2002 <- act_pobreza(archivo = "008_ENEMDU_Pob/202206_Tabulados_pobreza_junio_2022/202206_Tabulados_pobreza_EXCEL.xlsx", codigo_indicador = 2002, mes = 6, año = 2022, guardar = FALSE)
nuevo_2003 <- act_pobreza(archivo = "008_ENEMDU_Pob/202206_Tabulados_pobreza_junio_2022/202206_Tabulados_pobreza_EXCEL.xlsx", codigo_indicador = 2003, mes = 6, año = 2022, guardar = FALSE)
nuevo_2004 <- act_pobreza_multi(archivo = "008_ENEMDU_Pob/Tabulados IPM-dic 21.xlsx", codigo_indicador = 2004, año = 2021, guardar = FALSE)
nuevo_2005 <- act_pobreza_multi(archivo = "008_ENEMDU_Pob/Tabulados IPM-dic 21.xlsx", codigo_indicador = 2005, año = 2021, guardar = FALSE)
nuevo_2018 <- act_protestas(archivo = "025_ACLED/2017-01-01-2022-08-31-South_America-Ecuador.csv", mes = 8, año = 2022, guardar = FALSE)

# Estabilidad económica

nuevo_3001 <- act_enemdu_empleo(archivo = "002_ENEMDU/202207_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3001, guardar = FALSE)
nuevo_3002 <- act_enemdu_empleo(archivo = "002_ENEMDU/202207_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3002, guardar = FALSE)
nuevo_3003 <- act_enemdu_empleo(archivo = "002_ENEMDU/202207_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3003, guardar = FALSE)
nuevo_3024 <- act_CFB(archivo = "001_CFB/4. Ipc_canastabasica_nacional_ciudades_ago_2022.xls", mes = 8, año = 2022, guardar = FALSE)
nuevo_3025 <- act_enemdu_empleo(archivo = "002_ENEMDU/202207_Tabulados_Mercado_Laboral_EXCEL.xlsx", codigo_indicador = 3025, guardar = FALSE)



### Exportar base de datos

exportar_xlsx("2") #Cohesión social, equidad y no-discriminación
exportar_xlsx("3") #Estabilidad económica

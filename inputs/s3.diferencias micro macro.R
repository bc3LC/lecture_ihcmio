# Title       : Curso de Integración de Microdatos de Consumo de Hogares en Modelos Input-Output ----
# Author      : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Compute data to compare micro and macro gaps in household consumption data for Spain in 2019

# [] Preliminaries ----

# Clear workspace
rm(list = ls(all = TRUE))

# Set language
Sys.setenv(LANG = "en")

# Install and load any required R-package
packages.loaded <- installed.packages()
packages.needed <- c ( "dplyr"       ,
                       "openxlsx"    ,
                       "httr"        ,
                       "downloader"  ,
                       "stringr"     ,
                       "here"        )
for ( p in packages.needed) {
  if (!p %in% row.names(packages.loaded)) install.packages(p)
  eval(bquote(library(.(p))))
}

# Define main path
path <- here()

# Set working directory
setwd(paste0(path))

# [] Load and process the EPF data ----

# Define vectors
files_sp <- c('EPFgastos', 'EPFhogar', 'EPFmhogar')
files_en <- c('hbs_e', 'hbs_h', 'hbs_m')
old_names <- c('GASTO', 'CANTIDAD')
new_names <- c('EUR_HE', 'HQ')

# Set working directory
setwd(paste0(path, "/epf"))

# Define year
year = 2019
  
# Set working directory
setwd(paste0(path, "/epf/", year, "/CSV"))

# Load data by year
for (f in 1:length(files_en)) {
  
  # Define file names
  original_file = files_sp[f]
  final_file    = files_en[f]
  
  # Load file
  if (year < 2016) {
    eval(parse(text = paste0(final_file,  " <- read.table('", original_file, "_", year, ".csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)")))
  } else {
    eval(parse(text = paste0(final_file,  " <- read.table('", original_file, "_", year, ".csv', header = TRUE, sep = '', stringsAsFactors = FALSE)")))
  }
  
}

# Harmonize variable "CODIGO"
if (typeof(hbs_e$CODIGO) == "integer"){
  hbs_e$CODIGO <- as.character(hbs_e$CODIGO)
  hbs_e[nchar(hbs_e$CODIGO)==4,]$CODIGO <- paste0("0",  hbs_e[nchar(hbs_e$CODIGO)==4,]$CODIGO)
}

# Create consumption data by type (expenditure & quantity)
for (c in 1:length(new_names)) {
  
  # Define parameters
  new_name = new_names[c]
  old_name = old_names[c]
  
  # Create consumption data
  if(c==1){
    eval(parse(text = paste0("consumption_data <-  hbs_e %>% mutate(CODIGO = str_sub(CODIGO) <- paste0('", new_name, "', CODIGO), ", new_name, " = ", old_name, "/FACTOR) %>% reshape2::dcast(NUMERO ~ CODIGO, value.var= '", new_name, "', fun.aggregate = sum)")))
  } else {
    eval(parse(text = paste0("consumption_data <- left_join(consumption_data, hbs_e %>% mutate(CODIGO = str_sub(CODIGO) <- paste0('", new_name, "', CODIGO), ", new_name, " = ", old_name, "/FACTOR) %>% reshape2::dcast(NUMERO ~ CODIGO, value.var= '", new_name, "', fun.aggregate = sum), by = 'NUMERO')")))
  }
  
}

# Check the creation of the consumption data is correct
if (!year %in% c(2016, 2020) & nrow(consumption_data) != nrow(hbs_h)){stop()} # For 2016 and 2020 there is one household that doesn't have consumption information

# Create total consumption variable
hbs_h <- hbs_h %>% mutate (EUR_HE00 = GASTOT/FACTOR)

# Transfer consumption data to household data file
hbs_h <- left_join(hbs_h, consumption_data, by = "NUMERO")

# Check procedure
if (round(sum(hbs_h$EUR_HE00, na.rm = TRUE)) == round(sum(rowSums(dplyr::select(consumption_data, contains('EUR_HE'))), na.rm = TRUE))){
  print(paste0(year, ": Done correctly")) } else { stop()}

# Create vectors with COICOP names
coicop_names <- c('1.1 Alimentos', 
                  '1.2 Bebidas no alcohólicas', 
                  '2.1 Bebidas alcohólicas',
                  '2.2 Tabaco', 
                  '2.3 Narcóticos', 
                  '3.1 Artículos de vestir', 
                  '3.2 Calzado', 
                  '4.1 Alquileres reales de la vivienda',
                  '4.2 Alquileres imputados de la vivienda',
                  '4.3 Mantenimiento y reparación de la vivienda',
                  '4.4 Suministro de agua y servicios diversos relacionados con la vivienda',
                  '4.5 Electricidad, gas y otros combustibles', 
                  '5.1 Muebles, artículos de amueblamiento, alfombras y otros revestimientos para suelos y sus reparaciones', 
                  '5.2 Artículos textiles para el hogar ',
                  '5.3 Electrodomésticos para el hogar', 
                  '5.4 Cristalería, vajilla y utensilios del hogar', 
                  '5.5 Herramientas para casa y jardín', 
                  '5.6 Bienes y servicios para el mantenimiento corriente del hogar', 
                  '6.1 Medicamentos y otros productos farmacéuticos, aparatos y material terapéutico', 
                  '6.2 Servicios médicos y paramédicos extrahospitalarios', 
                  '6.3 Servicios hospitalarios', 
                  '7.1 Compra de vehículos', 
                  '7.2 Utilización de vehículos personales',
                  '7.3 Servicios de transporte', 
                  '8.1 Servicios Postales', 
                  '8.2 Equipos de teléfono y fax', 
                  '8.3 Servicios de teléfono y fax', 
                  '9.1 Equipos y accesorios audiovisuales, fotográficos y de procesamiento de información', 
                  '9.2 Otros bienes duraderos importantes para el ocio y la cultura', 
                  '9.3 Otros artículos y equipamientos recreativos, flores, jardinería y mascotas',
                  '9.4 Servicios recreativos y culturales', 
                  '9.5 Prensa, librería y papelería', 
                  '9.6 Vacaciones todo incluido', 
                  '10.1 Educación infantil y primaria', 
                  '10.2 Enseñanza secundaria',
                  '10.3 Enseñanza post-secundaria no terciaria', 
                  '10.4 Enseñanza terciaria', 
                  '10.5 Enseñanza no definida por el grado',
                  '11.1 Restaurantes y cafés', 
                  '11.2 Servicios de alojamiento', 
                  '12.1 Cuidados personales',
                  '12.2 Prostitución', 
                  '12.3 Efectos personales no declarados anteriormente', 
                  '12.4 Protección social', 
                  '12.5 Servicios de seguro', 
                  '12.6 Servicios financieros no declarados en otra parte', 
                  '12.7 Otros servicios')

# Create vectors with COICOP codes
coicop_codes <- c('EUR_HE011',
                  'EUR_HE012',
                  'EUR_HE021',
                  'EUR_HE022',
                  'EUR_HE023',
                  'EUR_HE031',
                  'EUR_HE032',
                  'EUR_HE041',
                  'EUR_HE042',
                  'EUR_HE043',
                  'EUR_HE044',
                  'EUR_HE045',
                  'EUR_HE051',
                  'EUR_HE052',
                  'EUR_HE053',
                  'EUR_HE054',
                  'EUR_HE055',
                  'EUR_HE056',
                  'EUR_HE061',
                  'EUR_HE062',
                  'EUR_HE063',
                  'EUR_HE071',
                  'EUR_HE072',
                  'EUR_HE073',
                  'EUR_HE081',
                  'EUR_HE082',
                  'EUR_HE083',
                  'EUR_HE091',
                  'EUR_HE092',
                  'EUR_HE093',
                  'EUR_HE094',
                  'EUR_HE095',
                  'EUR_HE096',
                  'EUR_HE101',
                  'EUR_HE102',
                  'EUR_HE103',
                  'EUR_HE104',
                  'EUR_HE105',
                  'EUR_HE111',
                  'EUR_HE112',
                  'EUR_HE121',
                  'EUR_HE122',
                  'EUR_HE123',
                  'EUR_HE124',
                  'EUR_HE125',
                  'EUR_HE126',
                  'EUR_HE127')

# Create variables with consumption categories by COICOP (3 digits)

# 1.1 Alimentos
EUR_HE011 <- names(dplyr::select(consumption_data, contains('EUR_HE011')))
hbs_h <- mutate(hbs_h, EUR_HE011 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE011)), na.rm = TRUE))

# 1.2 Bebidas no alcohólicas
EUR_HE012 <- names(dplyr::select(consumption_data, contains('EUR_HE012')))
hbs_h <- mutate(hbs_h, EUR_HE012 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE012)), na.rm = TRUE))

# 2.1 Bebidas alcohólicas
EUR_HE021 <- names(dplyr::select(consumption_data, contains('EUR_HE021')))
hbs_h <- mutate(hbs_h, EUR_HE021 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE021)), na.rm = TRUE))

# 2.2 Tabaco
EUR_HE022 <- names(dplyr::select(consumption_data, contains('EUR_HE022')))
hbs_h <- mutate(hbs_h, EUR_HE022 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE022)), na.rm = TRUE))

# 2.3 Narcóticos
EUR_HE023 <- names(dplyr::select(consumption_data, contains('EUR_HE023'))) # Sin información en la EPF
hbs_h <- mutate(hbs_h, EUR_HE023 = NA)

# 3.1 Artículos de vestir
EUR_HE031 <- names(dplyr::select(consumption_data, contains('EUR_HE031')))
hbs_h <- mutate(hbs_h, EUR_HE031 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE031)), na.rm = TRUE))

# 3.2 Calzado
EUR_HE032 <- names(dplyr::select(consumption_data, contains('EUR_HE032')))
hbs_h <- mutate(hbs_h, EUR_HE032 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE032)), na.rm = TRUE))

# 4.1 Alquileres reales de la vivienda
EUR_HE041 <- names(dplyr::select(consumption_data, contains('EUR_HE041')))
hbs_h <- mutate(hbs_h, EUR_HE041 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE041)), na.rm = TRUE))

# 4.2 Alquileres imputados de la vivienda
EUR_HE042 <- names(dplyr::select(consumption_data, contains('EUR_HE042')))
hbs_h <- mutate(hbs_h, EUR_HE042 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE042)), na.rm = TRUE))

# 4.3 Mantenimiento y reparación de la vivienda
EUR_HE043 <- names(dplyr::select(consumption_data, contains('EUR_HE043')))
hbs_h <- mutate(hbs_h, EUR_HE043 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE043)), na.rm = TRUE))

# 4.4 Suministro de agua y servicios diversos relacionados con la vivienda
EUR_HE044 <- names(dplyr::select(consumption_data, contains('EUR_HE044')))
hbs_h <- mutate(hbs_h, EUR_HE044 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE044)), na.rm = TRUE))

# 4.5 Electricidad, gas y otros combustibles
EUR_HE045 <- names(dplyr::select(consumption_data, contains('EUR_HE045')))
hbs_h <- mutate(hbs_h, EUR_HE045 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE045)), na.rm = TRUE))

# 5.1 Muebles, artículos de amueblamiento, alfombras y otros revestimientos para suelos y sus reparaciones
EUR_HE051 <- names(dplyr::select(consumption_data, contains('EUR_HE051')))
hbs_h <- mutate(hbs_h, EUR_HE051 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE051)), na.rm = TRUE))

# 5.2 Artículos textiles para el hogar
EUR_HE052 <- names(dplyr::select(consumption_data, contains('EUR_HE052')))
hbs_h <- mutate(hbs_h, EUR_HE052 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE052)), na.rm = TRUE))

# 5.3 Electrodomésticos para el hogar
EUR_HE053 <- names(dplyr::select(consumption_data, contains('EUR_HE053')))
hbs_h <- mutate(hbs_h, EUR_HE053 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE053)), na.rm = TRUE))

# 5.4 Cristalería, vajilla y utensilios del hogar
EUR_HE054 <- names(dplyr::select(consumption_data, contains('EUR_HE054')))
hbs_h <- mutate(hbs_h, EUR_HE054 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE054)), na.rm = TRUE))

# 5.5 Herramientas para casa y jardín
EUR_HE055 <- names(dplyr::select(consumption_data, contains('EUR_HE055')))
hbs_h <- mutate(hbs_h, EUR_HE055 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE055)), na.rm = TRUE))

# 5.6 Bienes y servicios para el mantenimiento corriente del hogar
EUR_HE056 <- names(dplyr::select(consumption_data, contains('EUR_HE056')))
hbs_h <- mutate(hbs_h, EUR_HE056 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE056)), na.rm = TRUE))

# 6.1 Medicamentos y otros productos farmacéuticos, aparatos y material terapéutico
EUR_HE061 <- names(dplyr::select(consumption_data, contains('EUR_HE061')))
hbs_h <- mutate(hbs_h, EUR_HE061 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE061)), na.rm = TRUE))

# 6.2 Servicios médicos y paramédicos extrahospitalarios
EUR_HE062 <- names(dplyr::select(consumption_data, contains('EUR_HE062')))
hbs_h <- mutate(hbs_h, EUR_HE062 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE062)), na.rm = TRUE))

# 6.3 Servicios hospitalarios
EUR_HE063 <- names(dplyr::select(consumption_data, contains('EUR_HE063')))
hbs_h <- mutate(hbs_h, EUR_HE063 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE063)), na.rm = TRUE))

# 7.1 Compra de vehículos
EUR_HE071 <- names(dplyr::select(consumption_data, contains('EUR_HE071')))
hbs_h <- mutate(hbs_h, EUR_HE071 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE071)), na.rm = TRUE))

# 7.2 Utilización de vehículos personales
EUR_HE072 <- names(dplyr::select(consumption_data, contains('EUR_HE072')))
hbs_h <- mutate(hbs_h, EUR_HE072 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE072)), na.rm = TRUE))

# 7.3 Servicios de transporte
EUR_HE073 <- names(dplyr::select(consumption_data, contains('EUR_HE073')))
hbs_h <- mutate(hbs_h, EUR_HE073 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE073)), na.rm = TRUE))

# 8.1 Servicios Postales
EUR_HE081 <- names(dplyr::select(consumption_data, contains('EUR_HE081')))
hbs_h <- mutate(hbs_h, EUR_HE081 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE081)), na.rm = TRUE))

# 8.2 Equipos de teléfono y fax
EUR_HE082 <- names(dplyr::select(consumption_data, contains('EUR_HE082')))
hbs_h <- mutate(hbs_h, EUR_HE082 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE082)), na.rm = TRUE))

# 8.3 Servicios de teléfono y fax
EUR_HE083 <- names(dplyr::select(consumption_data, contains('EUR_HE083')))
hbs_h <- mutate(hbs_h, EUR_HE083 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE083)), na.rm = TRUE))

# 9.1 Equipos y accesorios audiovisuales, fotográficos y de procesamiento de información
EUR_HE091 <- names(dplyr::select(consumption_data, contains('EUR_HE091')))
hbs_h <- mutate(hbs_h, EUR_HE091 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE091)), na.rm = TRUE))

# 9.2 Otros bienes duraderos importantes para el ocio y la cultura
EUR_HE092 <- names(dplyr::select(consumption_data, contains('EUR_HE092')))
hbs_h <- mutate(hbs_h, EUR_HE092 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE092)), na.rm = TRUE))

# 9.3 Otros artículos y equipamientos recreativos, flores, jardinería y mascotas
EUR_HE093 <- names(dplyr::select(consumption_data, contains('EUR_HE093')))
hbs_h <- mutate(hbs_h, EUR_HE093 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE093)), na.rm = TRUE))

# 9.4 Servicios recreativos y culturales
EUR_HE094 <- names(dplyr::select(consumption_data, contains('EUR_HE094')))
hbs_h <- mutate(hbs_h, EUR_HE094 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE094)), na.rm = TRUE))

# 9.5 Prensa, librería y papelería
EUR_HE095 <- names(dplyr::select(consumption_data, contains('EUR_HE095')))
hbs_h <- mutate(hbs_h, EUR_HE095 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE095)), na.rm = TRUE))

# 9.6 Vacaciones todo incluido
EUR_HE096 <- names(dplyr::select(consumption_data, contains('EUR_HE096')))
hbs_h <- mutate(hbs_h, EUR_HE096 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE096)), na.rm = TRUE))

# 10.1 Educación infantil y primaria
EUR_HE101 <- names(dplyr::select(consumption_data, contains('EUR_HE101')))
hbs_h <- mutate(hbs_h, EUR_HE101 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE101)), na.rm = TRUE))

# 10.2 Enseñanza secundaria
EUR_HE102 <- names(dplyr::select(consumption_data, contains('EUR_HE1021')))
hbs_h <- mutate(hbs_h, EUR_HE102 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE102)), na.rm = TRUE))

# 10.3 Enseñanza post-secundaria no terciaria
EUR_HE103 <- names(dplyr::select(consumption_data, contains('EUR_HE1022')))
hbs_h <- mutate(hbs_h, EUR_HE103 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE103)), na.rm = TRUE))

# 10.4 Enseñanza terciaria
EUR_HE104 <- names(dplyr::select(consumption_data, contains('EUR_HE103')))
hbs_h <- mutate(hbs_h, EUR_HE104 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE104)), na.rm = TRUE))

# 10.5 Enseñanza no definida por el grado
EUR_HE105 <- names(dplyr::select(consumption_data, contains('EUR_HE104')))
hbs_h <- mutate(hbs_h, EUR_HE105 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE105)), na.rm = TRUE))

# 11.1 Restaurantes y cafés
EUR_HE111 <- names(dplyr::select(consumption_data, contains('EUR_HE111')))
hbs_h <- mutate(hbs_h, EUR_HE111 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE111)), na.rm = TRUE))

# 11.2 Servicios de alojamiento
EUR_HE112 <- names(dplyr::select(consumption_data, contains('EUR_HE112')))
hbs_h <- mutate(hbs_h, EUR_HE112 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE112)), na.rm = TRUE))

# 12.1 Cuidados personales
EUR_HE121 <- names(dplyr::select(consumption_data, contains('EUR_HE121')))
hbs_h <- mutate(hbs_h, EUR_HE121 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE121)), na.rm = TRUE))

# 12.3 Prostitución
EUR_HE122 <- names(dplyr::select(consumption_data, contains('EUR_HE122'))) # Sin información en la EPF
hbs_h <- mutate(hbs_h, EUR_HE122 = NA)

# 12.3 Efectos personales no declarados anteriormente
EUR_HE123 <- names(dplyr::select(consumption_data, contains('EUR_HE123')))
hbs_h <- mutate(hbs_h, EUR_HE123 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE123)), na.rm = TRUE))

# 12.4 Protección social
EUR_HE124 <- names(dplyr::select(consumption_data, contains('EUR_HE124')))
hbs_h <- mutate(hbs_h, EUR_HE124 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE124)), na.rm = TRUE))

# 12.5 Servicios de seguro
EUR_HE125 <- names(dplyr::select(consumption_data, contains('EUR_HE125')))
hbs_h <- mutate(hbs_h, EUR_HE125 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE125)), na.rm = TRUE))

# 12.6 Servicios financieros no declarados en otra parte
EUR_HE126 <- names(dplyr::select(consumption_data, contains('EUR_HE126')))
hbs_h <- mutate(hbs_h, EUR_HE126 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE126)), na.rm = TRUE))

# 12.7 Otros servicios
EUR_HE127 <- names(dplyr::select(consumption_data, contains('EUR_HE127')))
hbs_h <- mutate(hbs_h, EUR_HE127 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE127)), na.rm = TRUE))

# 12.8 REMESAS Categoría incluida en la encuesta sin correspondencia clara en la contabilidad nacional
EUR_HE128 <- names(dplyr::select(consumption_data, contains('EUR_HE128')))
hbs_h <- mutate(hbs_h, EUR_HE128 = rowSums(dplyr::select(consumption_data, any_of(EUR_HE128)), na.rm = TRUE))

# Consumo total
hbs_h <- mutate(hbs_h, EUR_HE00   = EUR_HE011 +
                                    EUR_HE012 + 
                                    EUR_HE021 +
                                    EUR_HE022 +
                                   #EUR_HE023 + # Sin información en la EPF
                                    EUR_HE031 +
                                    EUR_HE032 +
                                    EUR_HE041 +
                                    EUR_HE042 +
                                    EUR_HE043 +
                                    EUR_HE044 +
                                    EUR_HE045 +
                                    EUR_HE051 +
                                    EUR_HE052 +
                                    EUR_HE053 +
                                    EUR_HE054 +
                                    EUR_HE055 +
                                    EUR_HE056 +
                                    EUR_HE061 +
                                    EUR_HE062 +
                                    EUR_HE063 +
                                    EUR_HE071 +
                                    EUR_HE072 +
                                    EUR_HE073 +
                                    EUR_HE081 +
                                    EUR_HE082 +
                                    EUR_HE083 +
                                    EUR_HE091 +
                                    EUR_HE092 +
                                    EUR_HE093 +
                                    EUR_HE094 +
                                    EUR_HE095 +
                                    EUR_HE096 +
                                    EUR_HE101 +
                                    EUR_HE102 +
                                    EUR_HE103 +
                                    EUR_HE104 +
                                    EUR_HE105 +
                                    EUR_HE111 +
                                    EUR_HE112 +
                                    EUR_HE121 +
                                    #EUR_HE122 + # Sin información en la EPF
                                    EUR_HE123 +
                                    EUR_HE124 +
                                    EUR_HE125 +
                                    EUR_HE126 +
                                    EUR_HE127)

# Create dataset with aggregated consumption from the EPF data
gcfh_epf_data <- data.frame (Descripcion = coicop_names,
                             COICOP      = coicop_codes,
                             ECH         = NA)

# Compute aggregated consumption expenditure by COICOP category
for (a in coicop_codes) {
  eval(parse(text = paste0("gcfh_epf_data[gcfh_epf_data$COICOP == '", a, "',]$ECH <- sum(hbs_h$", a, "*hbs_h$FACTOR)/1000000")))
}

# [] Load and process the GCFH data ---- 

# Set working directory
setwd(paste0(path, "/gcfh_cn"))

# Load data
gcfh_cn <- loadWorkbook("gcfhogares95_22.xlsx")

# Load the GCFH data
gcfh_cn_data <- read.xlsx(gcfh_cn, sheet = paste0("Tabla 1"), startRow = 7)

# Take data of interest
gcfh_cn_data <- gcfh_cn_data %>%
  dplyr::select(Divisiones.COICOP, `2019`) %>%
  rename(CN = `2019`, 
         Descripcion = Divisiones.COICOP)

# Take total consumption for the aggregated category: 12.2 y 12.7 Otros servicios
CN122.127 <- subset(gcfh_cn_data, Descripcion %in% c('12.2 y 12.7 Otros servicios')) %>% as.data.frame()
CN122.127 <- as.numeric(CN122.127$CN)

# Split the aggregated category: 12.2 y 12.7 Otros servicios into separate categories (assumption based on historical data: 12.2 represents 30% and 12.7 represents 70%).
CN122 = 0.3 * CN122.127     # 30% Supuesto basado en años previos
CN127 = CN122.127 - CN122

# Create a dataset with separated categories
CN122.127 <- data.frame (Descripcion = c('12.2 Prostitución', '12.7 Otros servicios'),
                         CN = c(CN122, CN127))

# Exclude categories not included in COICOP names vector
gcfh_cn_data <- subset(gcfh_cn_data, Descripcion %in% coicop_names) %>% as.data.frame() 

# Merge data
gcfh_cn_data <- rbind(gcfh_cn_data, CN122.127)

# Merge EPF and GCFH_CH data
data <- left_join(gcfh_epf_data, gcfh_cn_data, by = 'Descripcion') %>%
  mutate(RC = (ECH/CN)*100)

# Set working directory
setwd(paste0(path))

# Save results
material_2.4 <- createWorkbook()
addWorksheet(material_2.4, "datos")
writeData(material_2.4, sheet = paste0("datos"), data, xy = c(1,1), colNames = TRUE, rowNames = FALSE)
saveWorkbook(material_2.4, file = "material 2.4_discrepancias.xlsx", overwrite = TRUE)
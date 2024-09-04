# Title       : Curso de Integración de Microdatos de Consumo de Hogares en Modelos Input-Output ----
# Author      : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Download Spanish Households Final Consumption Expenditure by Consumption Purpose. 

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
setwd(paste0(path, "/inputs"))

# Delete the folder generated in previous runs (if it exists) 
if(dir.exists(paste0(path, "/inputs/gcfh_cn"))){unlink("gcfh_cn", recursive = TRUE)}

# Create gcfh_cn folder 
dir.create(paste0(path, "/inputs/gcfh_cn"))

# [] Download data from the Spanish Statistical Office ----

# Define file
eval(parse(text = paste0("file = 'gcfhogares95_22.xlsx'")))

# Define url
url <- "https://www.ine.es/daco/daco42/cne15/gcfhogares95_22.xlsx"

# Set the destination file
destination <- paste0(file)

# Set working directory
setwd(paste0(path, "/inputs/gcfh_cn"))

# Download microdata
download(url, destination,  mode = 'wb')
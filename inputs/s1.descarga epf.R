# Title       : Curso de Integración de Microdatos de Consumo de Hogares en Modelos Input-Output ----
# Author      : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Download Spanish Household Budget Survey microdata

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

# [] Download surveys from the Spanish Statistical Office ----
# This applies from 2016 onwards only. Previous years cannot be downloaded automatically.

# Define structural part of the url
base.url = 'https://www.ine.es/ftp/microdatos/epf2006/'

# Define initial year. 
initial.year = 2019

# Define final year
final.year = 2019

# By year
for (year in initial.year:final.year) {
  
  # Set working directory
  setwd(paste0(path, "/epf"))
  
  # Delete the folder generated in previous runs (if it exists) 
  if(dir.exists(paste0(path, "/epf/", year))){unlink(paste0(year), recursive = TRUE)}

  # Create epf folder 
  dir.create(paste0(path, "/epf/", year))

  # Define zip file
  eval(parse(text = paste0("file = 'datos_", year, ".zip'")))
  
  # Define url
  url <- paste0(base.url, file)
  
  # Set the destination file
  destination <- paste0(file)
  
  # Set working directory
  setwd(paste0(path, "/epf/", year))
  
  # Download microdata
  download(url, destination,  mode = 'wb')
  
  # Unzip microdata
  unzip(destination)
  
  # Delete unzipped folder
  unlink(destination, recursive = TRUE)
  
  # Create list of folders in the directory
  list.folders = list.files(pattern = '.zip', ignore.case = TRUE)
  
  # By folder
  for (f in 1:length(list.folders)) {
    
    # Define parameters
    folder = list.folders[f]

    # Unzip microdata
    unzip(folder)
    
    # Delete useless folder(s)
    unlink(folder, recursive = TRUE)
  }
}
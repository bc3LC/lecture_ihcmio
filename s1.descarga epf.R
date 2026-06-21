# Title       : Curso de Integración de Microdatos de Consumo de Hogares en Modelos Input-Output ----
# Author      : Manuel Tomas (manuel.tomas@bc3research.org)
# Institution : Basque Centre for Climate Change (BC3)

# [] Objective ----

# Download Spanish Household Budget Survey microdata

# [] Preliminaries ----

# Clear workspace
rm(list = ls(all = TRUE))

# Set language / encoding
# Sys.setenv(LANG = "en")
Sys.setlocale("LC_CTYPE", "Spanish_Spain.850")

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

# Create folder for the EPF
dir.create(paste0(path, "/inputs/epf"), recursive = TRUE, showWarnings = FALSE)

# By year
for (year in initial.year:final.year) {
  
  # Set working directory
  setwd(paste0(path, "/inputs"))
  
  # Set working directory to data folder
  setwd(file.path(path, "inputs"))
  
  # Delete the folder generated in previous runs if it exists
  year.folder <- file.path(path, "inputs", "epf", as.character(year))
  
  if (dir.exists(year.folder)) {
    unlink(year.folder, recursive = TRUE)
  }
  
  # Create year data folder
  dir.create(year.folder, recursive = TRUE)
  
  # Define zip file
  file <- paste0("datos_", year, ".zip")
  
  # Define URL
  url <- paste0(base.url, file)
  
  # Set destination file
  destination <- file.path(year.folder, file)
  
  # Download microdata
  downloader::download(
    url,
    destfile = destination,
    mode = "wb"
  )
  
  # Unzip main INE zip using archive
  # This avoids the "invalid multibyte string" error
  archive::archive_extract(
    archive = destination,
    dir = year.folder
  )
  
  # Delete main zip
  unlink(destination)
  
  # Create list of nested zip files
  list.folders <- list.files(
    path = year.folder,
    pattern = "\\.zip$",
    ignore.case = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  
  # Unzip nested zip files
  if (length(list.folders) > 0) {
    
    for (folder in list.folders) {
      
      archive::archive_extract(
        archive = folder,
        dir = dirname(folder)
      )
      
      # Delete nested zip
      unlink(folder)
    }
  }
}
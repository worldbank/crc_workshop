library(shiny)
library(leaflet)
library(tidyverse)
library(raster)
library(sf)
library(rgdal)

# Read raster function 
read_raster <- function(folder, raster_name, raster_band = NULL) {
  if (!is.null(raster_band)) {
    raster(paste0(folder, '/', raster_name, '.tif'), band = raster_band)
  } else {
    raster(paste0(folder, '/', raster_name, '.tif'))
  }
}

# Icons
icon_dim <- 20
osm_icon <- function(url) {
  makeIcon(iconUrl = url, iconWidth = icon_dim, iconHeight = icon_dim,
           iconAnchorX = icon_dim/2, iconAnchorY = icon_dim/2)
}

## fire
fire_icon <- osm_icon('fire.png')
## health
health_icon <- osm_icon('health.png')
## police
police_icon <- osm_icon('police.png')
## schools
schools_icon <- osm_icon('schools.png')

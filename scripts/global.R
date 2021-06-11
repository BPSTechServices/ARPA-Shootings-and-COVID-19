library(tidyverse)
library(tidycensus)
library(sf)
library(mapview)
library(tigris)
library(geojsonsf)
library(areal)
library(effectsize)
library(corrplot)
library(GGally)
library(factoextra)
library(biscale)
library(cowplot)
library(extrafont) #; font_import() 
library(viridis)
library(tmaptools)
library(tmap)
tmap_mode("view")

options(
  tigris_use_cache = T,
  tigris_class = "sf"
)

range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

sfc_as_cols <- function(x, names = c("lon","lat")) {
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

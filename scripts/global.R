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

options(
  tigris_use_cache = T,
  tigris_class = "sf"
)
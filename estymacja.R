library(sf)
library(stars)
library(gstat)
library(geostatbook) #testing
pomiary = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")


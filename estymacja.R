library(sf)
library(stars)
library(gstat)
library(geostatbook) #testing
library(tmap)
library(ggplot2)
train = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")
elev = read_stars("dane/elev.tif")
lc = read_stars("dane/lc.tif")

library(readxl)
lc_legend = read_excel("dane/lc_legend.xls")

tmap_mode("view")
tm_shape(siatka) +
  tm_raster() +
  tm_shape(train_clean)+
  tm_dots(col = "PM10", size = 0.4)
#dwa punkty odstajace
ggplot(train_clean, aes(PM10)) +geom_histogram()
#nom dwa punkty odstaja

siatka_poznan = st_as_sf(siatka)
train_clean = train[!(train$PM10== -40 | train$PM10 == -1),]
train_clean$intersects <- st_intersects(train_cleaner, siatka_poznan) %>% lengths > 0
train_clean = train_clean[(train_clean$intersects == T),]

tmap_mode("view")
tm_shape(siatka) +
  tm_raster() +
  tm_shape(train_clean)+
  tm_dots(col = "PM10", size = 0.4)
  
#dociac punkty tylko do siatki poznania?

tm_shape(elev) +
  tm_raster() +
  tm_shape(siatka) +
  tm_raster(alpha = 0) +
  tm_shape(train_clean) +
  tm_dots(col = "PM10", size = 0.3)

sv_simple = variogram(PM10~1, locations = train_clean)
plot(sv_simple)

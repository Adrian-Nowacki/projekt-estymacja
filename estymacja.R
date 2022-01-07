library(sf)
library(stars)
library(gstat)
library(geostatbook) #testing
library(tmap)
library(ggplot2)
library(readxl)
lc_legend = read_excel("dane/lc_legend.xls")
train = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")
elev = read_stars("dane/elev.tif")
lc = read_stars("dane/lc.tif")

#tworzenie zlaczonej siatki
siatka_join = c(siatka, lc, elev)

#tego na dole raczej nie potrzeba, jak nie wyjdzie estymacja to odkomentowac
#siatka_join$lc.tif = as.factor(siatka_join$lc.tif)
#siatka_join$elev.tif = as.factor(siatka_join$elev.tif)

#podglad danych punktowych z pusta siatka
tmap_mode("view")
tm_shape(siatka) +
  tm_raster() +
  tm_shape(train)+
  tm_dots(col = "PM10", size = 0.4)

#histogram danych wejsciowych
ggplot(train, aes(PM10)) +geom_histogram()

#utworzenie nowej zmiennej train_clean, z ktorej usunieto odstajace dane
train_clean = train[!(train$PM10== -40 | train$PM10 == -1),]

#utworzenie poligonu siatka_poznan z wejsciowej siatki, i dociecie punktow do obszaru siatki
siatka_poznan = st_as_sf(siatka)
train_clean$intersects <- st_intersects(train_clean, siatka_poznan) %>% lengths > 0
train_clean = train_clean[(train_clean$intersects == T),]

#histogram przeczyszczonych danychv
ggplot(train_clean, aes(PM10)) +geom_histogram()

#podglad na przeczyszczone dane
tmap_mode("view") 
tm_shape(siatka) +
  tm_raster() +
  tm_shape(train_clean)+
  tm_dots(col = "PM10", size = 0.4)

#dolaczenie do przeczyszczonych danych wartosci rastrow  
train_clean = st_join(train_clean, st_as_sf(lc))
train_clean = st_join(train_clean, st_as_sf(elev))


#tworzenie semiwariogramów i modeli do estymacji jednozmiennej
vario_jedno = variogram(PM10~1, locations = train_clean)
model_jednoSph = fit.variogram(vario_jedno, vgm(37, model = "Sph", nugget = 10))
plot(vario_jedno, model_jednoSph)

ocena_jedno = krige.cv(PM10 ~ 1,
                       locations = train_clean,
                       model = model_jednoSph,
                       nmax = 20)
RMSE = sqrt(mean((ocena_jedno$residual) ^ 2))
RMSE #4.77

#estymacja dla modelu jednozmiennego
krige_jedno = krige(PM10 ~ 1,
                    locations = train_clean,
                    newdata = siatka,
                    model = model_jednoSph,
                    beta = 32)

tm_shape(krige_jedno) +
  tm_raster(col = c("var1.pred", "var1.var"),
            style = "cont", 
            palette = list("-Spectral", "viridis")) +
  tm_layout(legend.frame = TRUE)

#tworzenie semiwariogramów i modeli do estymacji wielozmiennej
vario_wielo_lc = variogram(PM10~lc.tif, locations = train_clean)
model_wieloSph_lc = fit.variogram(vario_wielo_lc, vgm(model = "Sph"))
plot(vario_wielo_lc, model_wieloSph_lc)

ocena_wielo_lc = krige.cv(PM10 ~ lc.tif,
                         locations = train_clean,
                         model = model_wieloSph_lc
                         )
RMSE = sqrt(mean((ocena_wielo_lc$residual) ^ 2))
RMSE #5.02

vario_wielo_elev = variogram(PM10~elev.tif, locations = train_clean)
model_wieloSph_elev = fit.variogram(vario_wielo_elev, vgm(model = "Sph"))

ocena_wielo_elev = krige.cv(PM10 ~ elev.tif,
                           locations = train_clean,
                           model = model_wieloSph_elev
                           )

RMSE = sqrt(mean((ocena_wielo_elev$residual) ^ 2))
RMSE #4.92


k_lc_elev= krige(PM10 ~  elev.tif,
                 locations = train_clean, 
                 newdata = siatka_join,
                 model = model_wieloSph_elev)

vario_wielo = variogram(PM10 ~ lc.tif + elev.tif, locations = train_clean)
model_wieloSph = fit.variogram(vario_wielo, vgm(model = "Sph"))
plot(vario_wielo, model_wieloSph)

ocena_wielo = krige.cv(PM10 ~ lc.tif + elev.tif,
                       locations = train_clean,
                       model = model_wieloSph)
RMSE = sqrt(mean((ocena_wielo$residual) ^ 2))
RMSE #5.08


k_lc = krige(PM10 ~ lc.tif + elev.tif,
            locations = train_clean, 
            newdata = siatka_join,
            model = model_wieloSph)


tmap_mode("view")
tm_shape(k_lc) +
  tm_raster(col = c("var1.pred", "var1.var"),
            style = "cont", 
            palette = list("-Spectral", "viridis")) +
  tm_layout(legend.frame = TRUE)


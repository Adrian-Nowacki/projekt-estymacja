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

siatka_join = c(siatka, lc, elev)
siatka_join$lc.tif = as.factor(siatka_join$lc.tif)
siatka_join$elev.tif = as.factor(siatka_join$elev.tif)

tm_shape(siatka_join["elev.tif"]) +
  tm_raster()

library(readxl)
lc_legend = read_excel("dane/lc_legend.xls")

tmap_mode("view")
tm_shape(siatka) +
  tm_raster() +
  tm_shape(train)+
  tm_dots(col = "PM10", size = 0.4)

#dwa punkty odstajace
ggplot(train, aes(PM10)) +geom_histogram()
#nom dwa punkty odstaja


train_clean = train[!(train$PM10== -40 | train$PM10 == -1),]
siatka_poznan = st_as_sf(siatka)
train_clean$intersects <- st_intersects(train_cleaner, siatka_poznan) %>% lengths > 0
train_clean = train_clean[(train_clean$intersects == T),]

tmap_mode("view") 
tm_shape(siatka) +
  tm_raster() +
  tm_shape(train_clean)+
  tm_dots(col = "PM10", size = 0.4)
  
train_clean = st_join(train_clean, st_as_sf(lc))
train_clean = st_join(train_clean, st_as_sf(elev))

ggplot(train_clean, aes(PM10)) +geom_histogram()

#tworzenie semiwariogramów i modeli do estymacji jednozmiennej
sv_simple = variogram(PM10~1, locations = train_clean)

sph_simple = fit.variogram(sv_simple, vgm(37, model = "Sph", nugget = 10))
plot(sv_simple, sph_simple)

#ocena_sph = krige.cv(PM10 ~ 1,
                 #locations = train_clean,
                 #model = sph_simple,
                 #nmax = 20)
#RMSE = sqrt(mean((ocena_sph$residual) ^ 2))
#RMSE #4.77

k_simple = krige(PM10~1,
                 locations = train_clean,
                 newdata = siatka,
                 model = sph_simple,
                 beta = 32)

tm_shape(k_simple) +
  tm_raster(col = c("var1.pred", "var1.var"),
            style = "cont", 
            palette = list("-Spectral", "viridis")) +
  tm_layout(legend.frame = TRUE)

#tworzenie semiwariogramów i modeli do estymacji wielozmiennej
sv_hard = variogram(PM10~lc.tif + elev.tif, locations = train_clean)
plot(sv_hard)

sph_hard = fit.variogram(sv_hard, vgm(model = "Sph"))
plot(sv_hard, sph_hard)

ocena_hard = krige.cv(PM10 ~ lc.tif + elev.tif,
                     locations = train_clean,
                     model = sph_hard)
RMSE = sqrt(mean((ocena_hard$residual) ^ 2))
RMSE # 6.2 Sph, 6.12 Nug, 5.08 Sph bez nmax

sv_hard_lc = variogram(PM10~lc.tif, locations = train_clean)
plot(sv_hard_lc)

sph_hard_lc = fit.variogram(sv_hard_lc, vgm(model = "Sph"))
plot(sv_hard_lc, sph_hard_lc)

ocena_hard_lc = krige.cv(PM10 ~ lc.tif,
                      locations = train_clean,
                      model = sph_hard_lc
                      )
RMSE = sqrt(mean((ocena_hard_lc$residual) ^ 2))
RMSE #6.32 Sph, 6.27 Nug. 4.99 Nug bez nmax

sv_hard_elev = variogram(PM10~elev.tif, locations = train_clean)
sph_hard_elev = fit.variogram(sv_hard_elev, vgm(model = "Sph"))

ocena_hard_elev = krige.cv(PM10 ~ elev.tif,
                         locations = train_clean,
                         model = sph_hard_elev)

RMSE = sqrt(mean((ocena_hard_elev$residual) ^ 2))
RMSE #6.32 Sph, 6.27 Nug. 4.91 Sph bez nmax


k_lc_elev= krige(PM10 ~  elev.tif,
            locations = train_clean, 
            newdata = siatka_join,
            model = sph_hard)

k_lc= krige(PM10 ~ lc.tif + elev.tif,
                 locations = train_clean, 
                 newdata = siatka_join,
                 model = sph_hard)

#TO DZIAŁA!!!!!!!!!!!!!!!!!
tmap_mode("plot")
tm_shape(k_lc) +
  tm_raster(col = c("var1.pred", "var1.var"),
            style = "cont", 
            palette = list("-Spectral", "viridis")) +
  tm_layout(legend.frame = TRUE)



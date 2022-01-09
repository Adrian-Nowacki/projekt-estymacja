library(sf)
library(stars)
library(gstat)
library(tmap)
library(ggplot2)
library(readxl)

#wczytanie danych
lc_legend = read_excel("dane/lc_legend.xls")
train = read_sf("dane/train.gpkg")
siatka = read_stars("dane/pusta_siatka.tif")
elev = read_stars("dane/elev.tif")
lc = read_stars("dane/lc.tif")

#Wygenerowanie histogramu rozkladu wartosci zmiennej PM10
ggplot(train, aes(PM10)) + geom_histogram()

#Podglad rozkladu przestrzennego danych
tmap_mode("view")
tm_shape(siatka) +
  tm_raster() +
  tm_shape(train)+
  tm_dots(col = "PM10", size = 0.4)

#Utworzenie zmiennej train_clean, z ktorej usunieto bledne pomiary
train_clean = train[!(train$PM10== -40 | train$PM10 == -1),]

#Usuniecie pomiarow znajdujacych sie poza obszarem rastrow
siatka_tif = st_as_sf(lc)
train_clean$intersects <- st_intersects(train_clean, siatka_tif) %>% lengths > 0
train_clean = train_clean[(train_clean$intersects == T),]

#Przygotowanie danych do tworzenia modeli i estymacji
#Pobranie wartosci rastrow dla punktow pomiarowych
train_clean = st_join(train_clean, st_as_sf(lc))
train_clean = st_join(train_clean, st_as_sf(elev))

#Zlaczenie pustej siatki z rastrami dla krigingu uniwersalnego
siatka_join = c(siatka, lc, elev)

#Konstrukcja semiwariogramow i modelu do estymacji
vario_wielo = variogram(PM10 ~ lc.tif + elev.tif, locations = train_clean)
model_wieloSph = fit.variogram(vario_wielo, vgm(model = "Sph"))
plot(vario_wielo, model_wieloSph)

#Ocena modelu
ocena_wielo = krige.cv(PM10 ~ lc.tif + elev.tif,
                       locations = train_clean,
                       model = model_wieloSph)
RMSE = sqrt(mean((ocena_wielo$residual) ^ 2))
RMSE #4.96

#Estymacja przy uzyciu krigingu uniwersalnego
k_lc_elev = krige(PM10 ~ lc.tif + elev.tif,
                  locations = train_clean, 
                  newdata = siatka_join,
                  model = model_wieloSph)

tmap_mode("plot")
tm_shape(k_lc_elev) +
  tm_raster(col = c("var1.pred", "var1.var"),
            style = "cont", 
            palette = "-Spectral") +
  tm_layout(legend.frame = TRUE) 

#Zapisywanie wynikow
write.csv(RMSE, "Nowacki_Rydzik.csv")
write_stars(k_lc_elev["var1.pred"], "Nowacki_Rydzik.tif")

#Dodatkowe, wizualizacja estymacji
#Dodanie granic Poznania
pzn_borders = read_sf("dane/poznan.gpkg")

#dDdanie atrybutów mapy oraz stylizacji
tm_shape(k_lc_elev["var1.pred"]) +
  tm_raster(col = c("var1.pred"),
            style = "cont", 
            palette = "-Spectral",
            title = "  Predykcja PM10 \n         [µg/m3]",
            n = 7) +
  tm_shape(pzn_borders) +
  tm_borders(col = "#555555",
             lwd = 2) +
  
  tm_compass(position = c(0.87, 0.87),
             text.size = .6) +
  tm_scale_bar(width = 0.25,
               text.size = 0.8,
               position = c(0.3, 0.005, 0, 0)) +
  tm_layout("Estymowane wartości pyłu zawieszonego\n        PM10 na terenie miasta Poznań", 
            title.position = c(0.005, 0.97, 0, 0),
            title.color = "#ffffff",
            title.bg.color = "#333333",
            title.bg.alpha = 0.8,
            title.size = 1.05,
            legend.outside = FALSE, 
            legend.text.color = "#ffffff",
            legend.title.color = "#ffffff",
            legend.text.size = 0.9, 
            legend.title.size = 1.3,
            legend.position = c(0.02, 0.02, 0, 0),
            legend.bg.color = "#333333",
            legend.bg.alpha = 0.8,
            compass.type = "rose"
  ) + 
  tm_add_legend(
    type = "line",
    labels = "granica Poznania",
    col = "#222222",
    lwd = 3)

install.packages("raster")
install.packages("ncdf4")
library(raster)
library(ncdf4)
long_lat <- read.csv("estaciones_c.csv", header = T)
View(long_lat)
raster_pp   <- raster::brick("Prec.nc")
raster_tmax <- raster::brick("Tmax.nc")
raster_tmin <- raster::brick("Tmin.nc")
sp::coordinates(long_lat) <- ~XX+YY
raster::projection(long_lat) <- raster::projection(raster_pp)
raster::projection(long_lat) <- raster::projection(raster_tmax)
raster::projection(long_lat) <- raster::projection(raster_tmin)

points_long_lat_pp <- raster::extract(raster_pp[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_pp <- t(raster_pp[points_long_lat_pp])
colnames(data_long_lat_pp) <- as.character(long_lat$NN)
write.csv(data_long_lat_pp, "DATA PISCO/PP MENSUAL/prep.csv", quote = F)
#TEMPERATURA MAXIMA
points_long_lat_tmax <- raster::extract(raster_tmax[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmax <- t(raster_tmax[points_long_lat_tmax])
colnames(data_long_lat_tmax) <- as.character(long_lat$NN)
write.csv(data_long_lat_tmax, "DATA PISCO/TMAX/tmax.csv", quote = F)
#TEMPERATURA MINIMA
points_long_lat_tmin <- raster::extract(raster_tmin[[1]], long_lat, cellnumbers = T)[,1]
data_long_lat_tmin <- t(raster_tmin[points_long_lat_tmin])
colnames(data_long_lat_tmin) <- as.character(long_lat$NN)
write.csv(data_long_lat_tmin, "DATA PISCO/TMIN/tmin.csv", quote = F)
#TEMPERATURA MEDIA
tmedia <- (data_long_lat_tmax+data_long_lat_tmin)/2
write.csv(tmedia, "DATA EN CSV/PP ESTACIONES/tmedia.csv", quote = F)

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)
library(ggplot2)
library(dplyr)

Pp_pisco <- read.csv("DATA PISCO/PP MENSUAL/prep.csv", header = T, sep = ",") %>%
  tibble() %>%
  dplyr::select(-X) %>%
  mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
View(Pp_pisco)
write.csv(Pp_pisco, "DATA EN CSV/PP ESTACIONES/pp.csv")
colnames(Pp_pisco)

tmin_pisco <- read.csv("DATA PISCO/TMIN/tmin.csv", header = T, sep = ",") %>%
  tibble() %>%
  dplyr::select(-X) %>%
  mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
View(tmin_pisco)
write.csv(tmin_pisco, "DATA EN CSV/PP ESTACIONES/tmin.csv")
colnames(tmin_pisco)

tmax_pisco <- read.csv("DATA PISCO/TMIN/tmin.csv", header = T, sep = ",") %>%
  tibble() %>%
  dplyr::select(-X) %>%
  mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
View(tmax_pisco)
write.csv(tmax_pisco, "DATA EN CSV/PP ESTACIONES/tmax.csv")
colnames(tmax_pisco)

tmedia_pisco <- read.csv("DATA EN CSV/PP ESTACIONES/tmedia.csv", header = T, sep = ",") %>%
  tibble() %>%
  dplyr::select(-X) %>%
  mutate(fecha = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month"))
View(tmedia_pisco)
write.csv(tmedia_pisco, "DATA EN CSV/PP ESTACIONES/tmedia.csv")
colnames(tmedia_pisco)

pisac <- plot(Pp_pisco$fecha,Pp_pisco$PISAC, type = "l", col= 'blue',
              main= 'Serie de Tiempo de la estación Pisac', xlab= 'Año',
              ylab= 'Precipitación')

Paruro <- plot(Pp_pisco$fecha, Pp_pisco$PARURO, type = "l", col= 'blue',
               main= 'Serie de Tiempo de la estación Paruro', xlab= 'Año',
               ylab= 'Precipitación')

colquepata <- plot(Pp_pisco$fecha,Pp_pisco$COLQUEPATA, type = "l", col= 'blue',
                   main= 'Serie de Tiempo de la estación Colquepata', xlab= 'Año',
                   ylab= 'Precipitación')

catca <-  plot(Pp_pisco$fecha,Pp_pisco$CCATCCA, type = "l", col= 'blue',
               main= 'Serie de Tiempo de la estación Ccatcca', xlab= 'Año',
               ylab= 'Precipitación')

caycay <- plot(Pp_pisco$fecha,Pp_pisco$CAICAY, type = "l", col= 'blue',
               main= 'Serie de Tiempo de la estaci?n Cay Cay', xlab= 'Año',
               ylab= 'Precipitación')

########################################
#########graficas avanzadas###########
########################################
####precipitacion de nuestras 5 estaciones
library(ggplot2)
ggplot(Pp_pisco, aes(fecha, PISAC)) +
  geom_line() +
  geom_smooth(span = 0.4)

ggplot(Pp_pisco, aes(fecha, PARURO)) +
  geom_line() +
  geom_smooth(span = 0.4)

ggplot(Pp_pisco, aes(fecha, COLQUEPATA)) +
  geom_line() +
  geom_smooth(span = 0.5)

ggplot(Pp_pisco, aes(fecha, CAICAY)) +
  geom_line() +
  geom_smooth(span = 0.5)

ggplot(Pp_pisco, aes(fecha, CCATCCA)) +
  geom_line() +
  geom_smooth(span = 0.5)

###########Poligonos de frecuencia para temperaturas
#Pisac
ggplot(tmedia_pisco, aes(PISAC)) + geom_histogram(colour= 'blue')
ggplot(tmedia_pisco, aes(PARURO)) + geom_histogram(colour= 'blue')
ggplot(tmedia_pisco, aes(COLQUEPATA)) + geom_histogram(colour= 'blue')
ggplot(tmedia_pisco, aes(CCATCCA)) + geom_histogram(colour= 'blue')
ggplot(tmedia_pisco, aes(CAICAY)) + geom_histogram(colour= 'blue')

ggplot(Pp_pisco, aes(fecha ,PISAC)) +
  geom_path(colour = "green")

ggplot(Pp_pisco, aes(fecha ,PARURO)) +
  geom_path(colour = "green")

ggplot(Pp_pisco, aes(fecha ,COLQUEPATA)) +
  geom_path(colour = "green")

ggplot(Pp_pisco, aes(fecha ,CCATCCA)) +
  geom_path(colour = "green")

ggplot(Pp_pisco, aes(fecha ,CAICAY)) +
  geom_path(colour = "green")

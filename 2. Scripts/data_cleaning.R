# Data Cleaning Base Training
# Problem_Set_3
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_3_G16/3. Stores")

## llamado librerías de la sesión
require(pacman)

p_load(tidyverse,rio,
       sf,
       leaflet,
       tmaptools,
       osmdata,
       nngeo)

###*** 1. llamado bases de datos ***###
train <- import("train.Rds") %>% mutate(base="train")
test <- import("test.Rds") %>% mutate(base="test")
house <- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)

leaflet() %>% addTiles() %>% addCircles(data=house)

str(house)

###*** 2. Obtención polígonos de interes ***###
chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)

st_crs(house)
st_crs(chapinero)

chapinero <- st_transform(chapinero,st_crs(house))

house_chapinero <- house[chapinero,]

leaflet() %>% addTiles() %>% addCircles(data=house_chapinero) %>% addPolygons(data = chapinero, col = "red")

poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon")

poblado <- st_transform(poblado,st_crs(house))

house_poblado <- house[poblado,]

leaflet() %>% addTiles() %>% addCircles(data=house_poblado) %>% addPolygons(data = poblado, col = "red")

available_features()

available_tags("amenity")

###*** 3. Traigo base de datos con manzanas de las 2 localidades ***###

mnz = import("mnz_urban.rds")

sf_use_s2(FALSE)

mnz_chapinero <- mnz[chapinero,]

leaflet() %>% addTiles() %>% addCircles(data=house_chapinero) %>% addPolygons(data = mnz_chapinero, col = "red")

mnz_poblado <- mnz[poblado,]

leaflet() %>% addTiles() %>% addCircles(data=house_poblado) %>% addPolygons(data = mnz_poblado, col = "red")

house_chapinero_mnz = st_join(house_chapinero,mnz_chapinero)

colnames(house_chapinero_mnz)

table(is.na(house_chapinero_mnz$COD_DANE_ANM))

house_poblado_mnz = st_join(house_poblado,mnz_poblado)

colnames(house_poblado_mnz)

table(is.na(house_poblado_mnz$COD_DANE_ANM))

leaflet() %>% addTiles() %>% addCircles(data=house) %>% addPolygons(data = mnz, col = "red")

###*** 4. Creación de variables con vecinos espaciales ***###

## Identificación de amenity
# Chapinero - distancia a bares
bar_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_chapinero , col="black")

# Poblado - distancia a bares
bar_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_poblado , col="black")

# Chapinero - distancia a estaciones de bus
osm_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_chapinero <- osm_chapinero %>% osmdata_sf()

bus_station_chapinero <- osm_sf_chapinero$osm_points %>% select(osm_id,amenity)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_chapinero , col="darkorange")

# Poblado - distancia a estaciones de bus
osm_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_poblado <- osm_poblado %>% osmdata_sf()

bus_station_poblado <- osm_sf_poblado$osm_points %>% select(osm_id,amenity)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_poblado , col="darkorange")

# Chapinero - distancia a bancos
bank_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bank_chapinero , col="deeppink1")

# Poblado - distancia a bancos
bank_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bank_poblado , col="deeppink1")

# Chapinero - distancia a restaurantes
restaurant_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant_chapinero , col="darkseagreen3")

# Poblado - distancia a restaurantes
restaurant_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant_poblado , col="darkseagreen3")

# Chapinero - distancia a escuelas
school_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=school_chapinero , col="green1")

# Poblado - distancia a escuelas
school_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=school_poblado , col="green1")

## creación de variables
house_chapinero_mnz$dist_bar <- st_distance(x = house_chapinero_mnz, y = bar_chapinero)
house_poblado_mnz$dist_bar <- st_distance(x = house_poblado_mnz, y = bar_poblado)

house_chapinero_mnz$dist_bus_station <- st_distance(x = house_chapinero_mnz, y = bus_station_chapinero)
house_poblado_mnz$dist_bus_station <- st_distance(x = house_poblado_mnz, y = bus_station_poblado)

house_chapinero_mnz$dist_bank <- st_distance(x = house_chapinero_mnz, y = bank_chapinero)
house_poblado_mnz$dist_bank <- st_distance(x = house_poblado_mnz, y = bank_poblado)

house_chapinero_mnz$dist_restaurant <- st_distance(x = house_chapinero_mnz, y = restaurant_chapinero)
house_poblado_mnz$dist_restaurant <- st_distance(x = house_poblado_mnz, y = restaurant_poblado)

house_chapinero_mnz$dist_school <- st_distance(x = house_chapinero_mnz, y = school_chapinero)
house_poblado_mnz$dist_school <- st_distance(x = house_poblado_mnz, y = school_poblado)

house_chapinero_mnz <- house_chapinero_mnz %>% mutate(Neighborhood = "Chapinero")

house_poblado_mnz <- house_poblado_mnz %>% mutate(Neighborhood = "Poblado")

house_mnz <- rbind(house_chapinero_mnz, house_poblado_mnz)

###*** 5. Rescate de texto como variables ***###

## convierto el texto de description a minúscula
str_to_lower(string = house_mnz$description)

## Patrones para rescatar texto de la variable "description"
# área de los inmuebles "area toral"
a = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2"
a1 = "[:space:]+[:digit:]+[:space:]+m2" 
a2 = "[:space:]+[:digit:]+m2" 
a3 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt2" 
a4 = "[:space:]+[:digit:]+[:space:]+mt2" 
a5 = "[:space:]+[:digit:]+mt2" 
a6 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros" 
a7 = "[:space:]+[:digit:]+[:space:]+metros" 
a8 = "[:space:]+[:digit:]+metros" 
a9 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros cuadrados" 
a10 = "[:space:]+[:digit:]+[:space:]+metros cuadrados" 
a11 = "[:space:]+[:digit:]+metros cuadrados" 
a12 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts" 
a13 = "[:space:]+[:digit:]+[:space:]+mts" 
a14 = "[:space:]+[:digit:]+mts" 
a15 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m^2" 
a16 = "[:space:]+[:digit:]+[:space:]+m^2" 
a17 = "[:space:]+[:digit:]+m^2" 
a18 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mtrs" 
a19 = "[:space:]+[:digit:]+[:space:]+mtrs" 
a20 = "[:space:]+[:digit:]+mtrs"
a21 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt" 
a22 = "[:space:]+[:digit:]+[:space:]+mt" 
a23 = "[:space:]+[:digit:]+mt"
a24 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts2" 
a25 = "[:space:]+[:digit:]+[:space:]+mts2" 
a26 = "[:space:]+[:digit:]+mts2"
a27 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metro cuadrados" 
a28 = "[:space:]+[:digit:]+[:space:]+metro cuadrados" 
a29 = "[:space:]+[:digit:]+metro cuadrados" 

# Se crea variable "area_total" con texto
house_mnz = house_mnz %>%
  mutate(area_total = str_extract
         (string=house_mnz$description ,
           pattern = paste0(a, "|", a1, "|", a2, "|", a3, "|", a4, "|", a5, "|", a6, "|", a7, "|", a8, "|", a9, "|", a10, "|", a11, "|", a12, "|", a13, "|", a14, "|", a15, "|", a16, "|", a17, "|", a18, "|", a19, "|", a20, "|", a21, "|", a22, "|", a23, "|", a24, "|", a25, "|", a26, "|", a27, "|", a28, "|", a29)))

# ajustes finales variable area_total
house_mnz$area_total <- str_replace_all (string=house_mnz$area_total, "[:punct:]" , replacement = ".")
house_mnz$area_total <- str_replace_all (string=house_mnz$area_total, "[:alpha:]+[:digit:]" , replacement = "")
house_mnz$area_total <- str_replace_all (string=house_mnz$area_total, "[:alpha:]" , replacement = "")

house_mnz$area_total <- as.numeric(house_mnz$area_total)

# número de habitaciones de los inmuebles
b = "[:space:]+[:digit:]+[:space:]+habitaciones"
b1 = "[:space:]+[:digit:]+habitaciones" 
b2 = "[:space:]+[:digit:]+[:space:]+alcobas" 
b3 = "[:space:]+[:digit:]+alcobas" 
b4 = "[:space:]+[:alpha:]+[:space:]+alcobas" 
b5 = "[:space:]+[:alpha:]+alcobas" 
b6 = "[:space:]+[:alpha:]+[:space:]+habitaciones" 
b7 = "[:space:]+[:alpha:]+habitaciones" 
b8 = "[:space:]+[:alpha:]+[:space:]+alcoba" 
b9 = "[:space:]+[:alpha:]+alcoba" 
b10 = "[:space:]+[:digit:]+[:space:]+alcoba" 
b11 = "[:space:]+[:digit:]+alcoba"
b12 = "[:space:]+[:alpha:]+[:space:]+habitación" 
b13 = "[:space:]+[:alpha:]+habitación"
b14 = "[:space:]+[:digit:]+[:space:]+habitación" 
b15 = "[:space:]+[:digit:]+habitación"
b16 = "[:space:]+[:alpha:]+[:space:]+habitacion" 
b17 = "[:space:]+[:alpha:]+habitacion"
b18 = "[:space:]+[:digit:]+[:space:]+habitacion" 
b19 = "[:space:]+[:digit:]+habitacion" 
b20 = "habitacion+[:space:]+[:alpha:]" 
b21 = "habitacion+[:space:]+[:digit:]"
b22 = "habitaciones+[:space:]+[:alpha:]" 
b23 = "habitaciones+[:space:]+[:digit:]"
b24 = "alcoba+[:space:]+[:alpha:]" 
b25 = "alcobas+[:space:]+[:digit:]"
b26 = "habitación+[:space:]+[:alpha:]" 
b27 = "habitación+[:space:]+[:digit:]"

house_mnz = house_mnz %>%
  mutate(alcobas = str_extract
         (string=house_mnz$description ,
           pattern = paste0(b, "|", b1, "|", b2, "|", b3, "|", b4, "|", b5, "|", b6, "|", b7, "|", b8, "|", b9, "|", b10, "|", b11, "|", b12, "|", b13, "|", b14, "|", b15, "|", b16, "|", b17, "|", b18, "|", b19, "|", b20, "|", b21, "|", b22, "|", b23, "|", b24, "|", b25, "|", b26, "|", b27)))

# ajustes finales variable alcobas
house_mnz$alcobas <- str_replace_all (string=house_mnz$alcobas, "[:punct:]" , replacement = ".")
house_mnz$alcobas <- str_replace_all (string=house_mnz$alcobas, "[:alpha:]+[:digit:]" , replacement = "")
house_mnz$alcobas <- str_replace_all (string=house_mnz$alcobas, "[:alpha:]" , replacement = "")

house_mnz$alcobas <- as.numeric(house_mnz$alcobas)

# baños
c = "[:space:]+[:digit:]+[:space:]+baos" 
c1 = "[:space:]+[:digit:]+baos" 
c2 = "[:space:]+[:alpha:]+[:space:]+baos"
c3 = "[:space:]+[:alpha:]+baos" 
c4 = "[:space:]+[:digit:]+[:space:]+bao" 
c5 = "[:space:]+[:digit:]+bao"
c6 = "[:space:]+[:alpha:]+[:space:]+bao"
c7 = "[:space:]+[:alpha:]+bao"
c8 = "[:space:]+[:digit:]+[:space:]+banio" 
c9 = "[:space:]+[:digit:]+banio"
c10 = "[:space:]+[:alpha:]+[:space:]+banio"
c11 = "[:space:]+[:alpha:]+banio"
c12 = "[:space:]+[:digit:]+[:space:]+bano" 
c13 = "[:space:]+[:digit:]+bano"
c14 = "[:space:]+[:alpha:]+[:space:]+bano"
c15 = "[:space:]+[:alpha:]+bano"
c16 = "[:space:]+[:digit:]+[:space:]+banios" 
c17 = "[:space:]+[:digit:]+banios"
c18 = "[:space:]+[:alpha:]+[:space:]+banios"
c19 = "[:space:]+[:alpha:]+banios"
c20 = "[:space:]+[:digit:]+[:space:]+banos" 
c21 = "[:space:]+[:digit:]+banos"
c22 = "[:space:]+[:alpha:]+[:space:]+banos"
c23 = "[:space:]+[:alpha:]+banos"
c24 = "[:space:]+[:digit:]+[:space:]+bañio" 
c25 = "[:space:]+[:digit:]+bañio"
c26 = "[:space:]+[:alpha:]+[:space:]+bañio"
c27 = "[:space:]+[:alpha:]+bañio"
c28 = "[:space:]+[:digit:]+[:space:]+bañios" 
c29 = "[:space:]+[:digit:]+bañios"
c30 = "[:space:]+[:alpha:]+[:space:]+bañios"
c31 = "[:space:]+[:alpha:]+bañios"
c32 = "[:space:]+[:digit:]+[:space:]+baños" 
c33 = "[:space:]+[:digit:]+baños"
c34 = "[:space:]+[:alpha:]+[:space:]+baños"
c35 = "[:space:]+[:alpha:]+baños"
c36 = "[:space:]+[:digit:]+[:space:]+baño" 
c37 = "[:space:]+[:digit:]+baño"
c38 = "[:space:]+[:alpha:]+[:space:]+baño"
c39 = "[:space:]+[:alpha:]+baño"

house_mnz = house_mnz %>%
  mutate(baños = str_extract
         (string=house_mnz$description ,
           pattern = paste0(c, "|", c1, "|", c2, "|", c3, "|", c4, "|", c5, "|", c6, "|", c7, "|", c8, "|", c9, "|", c10, "|", c11, "|", c12, "|", c13, "|", c14, "|", c15, "|", c16, "|", c17, "|", c18, "|", c19, "|", c20, "|", c21, "|", c22, "|", c23, "|", c24, "|", c25, "|", c26, "|", c27, "|", c28, "|", c29, "|", c30, "|", c31, "|", c32, "|", c33, "|", c34, "|", c35, "|", c36, "|", c37, "|", c38, "|", c39)))

# ajustes finales variable alcobas
house_mnz$baños <- str_replace_all (string=house_mnz$baños, "[:punct:]" , replacement = ".")
house_mnz$baños <- str_replace_all (string=house_mnz$baños, "[:alpha:]+[:digit:]" , replacement = "")
house_mnz$baños <- str_replace_all (string=house_mnz$baños, "[:alpha:]" , replacement = "")

house_mnz$baños <- as.numeric(house_mnz$baños)

# parqueaderos
d = "[:space:]+[:digit:]+[:space:]+garaje" 
d1 = "[:space:]+[:digit:]+garaje" 
d2 = "[:space:]+[:alpha:]+[:space:]+garaje"
d3 = "[:space:]+[:alpha:]+garaje" 
d4 = "garaje+[:space:]+[:digit:]" 
d5 = "garaje+[:space:]+[:alpha:]"
d6 = "garaje+[:digit:]" 
d7 = "garaje+[:alpha:]"
d = "[:space:]+[:digit:]+[:space:]+garajes" 
d1 = "[:space:]+[:digit:]+garajes" 
d2 = "[:space:]+[:alpha:]+[:space:]+garajes"
d3 = "[:space:]+[:alpha:]+garajes" 
d4 = "garajes+[:space:]+[:digit:]" 
d5 = "garajes+[:space:]+[:alpha:]"
d6 = "garajes+[:digit:]" 
d7 = "garajes+[:alpha:]"


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
p_load(rio,tidyverse,sf,leaflet,class,skimr, osmdata)

### 1. llamado bases de datos ###
hogares <- import("train.Rds")
skim(hogares)
class(hogares)
table(hogares$l3)

hogares = st_as_sf(hogares,coords=c("lon","lat"),crs=4326)
class(hogares)

## 1.1 Obtención políginos de interes

chapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=chapinero)
train_ch <- st_crop (hogares, chapinero)
leaflet() %>% addTiles() %>% addPolygons(data=chapinero , color="red") %>% addCircles(data=train_ch)


poblado <- getbb(place_name = "Comuna 14 - El Poblado", 
                 featuretype = "boundary:administrative", 
                 format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=poblado)
train_pobl <- st_crop (hogares , poblado)
leaflet() %>% addTiles() %>% addPolygons(data=poblado , color="red") %>% addCircles(data=train_pobl)

## 1.2. Traigo base de datos con manzanas de las 2 localidades


parqueadero dummy 
ascensor 

### *** Rescate de texto como variables *** ###

## convierto el texto de description a minúscula
str_to_lower(string = trainF$description)

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
trainf = trainf %>%
  mutate(area_total = str_extract
         (string=trainF$description ,
           pattern = paste0(a, "|", a1, "|", a2, "|", a3, "|", a4, "|", a5, "|", a6, "|", a7, "|", a8, "|", a9, "|", a10, "|", a11, "|", a12, "|", a13, "|", a14, "|", a15, "|", a16, "|", a17, "|", a18, "|", a19, "|", a20, "|", a21, "|", a22, "|", a23, "|", a24, "|", a25, "|", a26, "|", a27, "|", a28, "|", a29)))

# ajustes finales variable area_total
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "," , replacement = ".")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "m2" , replacement = "")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "mt2" , replacement = ".")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "metros" , replacement = "")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "metros cuadrados" , replacement = ".")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "mts" , replacement = "")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "m^2" , replacement = ".")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "mtrs" , replacement = "")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "mt" , replacement = ".")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "mts2" , replacement = "")
trainF$area_total <- str_replace_all (string=trainF$area_total, patterns = "metro cuadrados" , replacement = "")

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

trainf = trainf %>%
  mutate(alcobas = str_extract
         (string=trainF$description ,
           pattern = paste0(b, "|", b1, "|", b2, "|", b3, "|", b4, "|", b5, "|", b6, "|", b7, "|", b8, "|", b9, "|", b10, "|", b11, "|", b12, "|", b13, "|", b14, "|", b15, "|", b16, "|", b17, "|", b18, "|", b19, "|", b20, "|", b21, "|", b22, "|", b23, "|", b24, "|", b25, "|", b26, "|", b27)))

trainF$alcobas <- str_replace_all (string=trainF$alcobas , patterns = "," , replacement = ".")
trainF$alcobas <- str_replace_all (string=trainF$alcobas , patterns = "habitaciones" , replacement = "")
trainF$alcobas <- str_replace_all (string=trainF$alcobas , patterns = "alcoba" , replacement = ".")
trainF$alcobas <- str_replace_all (string=trainF$alcobas , patterns = "alcobas" , replacement = "")
trainF$alcobas <- str_replace_all (string=trainF$alcobas , patterns = "habitación" , replacement = ".")
trainF$alcobas <- str_replace_all (string=trainF$alcobas , patterns = "habitacion" , replacement = "")

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

trainf = trainf %>%
  mutate(baños = str_extract
         (string=trainF$description ,
           pattern = paste0(c, "|", c1, "|", c2, "|", c3, "|", c4, "|", c5, "|", c6, "|", c7, "|", c8, "|", c9, "|", c10, "|", c11, "|", c12, "|", c13, "|", c14, "|", c15, "|", c16, "|", c17, "|", c18, "|", c19, "|", c20, "|", c21, "|", c22, "|", c23, "|", c24, "|", c25, "|", c26, "|", c27, "|", c28, "|", c29, "|", c30, "|", c31, "|", c32, "|", c33, "|", c34, "|", c35, "|", c36, "|", c37, "|", c38, "|", c39)))


trainF$baños <- str_replace_all (string=trainF$baños, patterns = "," , replacement = ".")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "baos" , replacement = "")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "bao" , replacement = ".")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "banio" , replacement = "")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "bano" , replacement = ".")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "banios" , replacement = "")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "banos" , replacement = ".")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "bañio" , replacement = "")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "bañios" , replacement = ".")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "baños" , replacement = "")
trainF$baños <- str_replace_all (string=trainF$baños, patterns = "baño" , replacement = "")

##=== 2. vecinos espaciales ===## 
trainmnz = st_join(x = trainf,y = mnz)

# vecinos espaciales por manzana para imputar valores de área total faltantes 
trainmnz = trainmnz %>%
  group_by(MANZ_CCNCT)
  mutate(area_total2=median(surface_total,na.rm=T))

# para baños y habitaciones se podría crear una variable de que sea como mínimo 1
 

### Creación variables finales
  
# area total
  
# habitaciones
  
# baños 
  
  
## Creación de variables con vecinos espaciales

## Estaciones de bus  
# 1. se define área de búsqueda 
## objeto osm
osm = chapinero(bbox = getbb(place_name = "UPZ Chapinero, Bogota"),
  add_osm_feature(key="amenity" , value="bus_station")) 
class(osm) # es una lista 

## extraer Simple Features Collection
osm_sf
## Obtener un objeto sf
bus_station = osm_sf$osm_points %>% select(osm_id,amenity) # coje el objeto osm_sf y extrae los puntos, el final le doy selec y me quedo con sólo dos variables de las estaciones de buses, que es dodne esta ubicado y el nombre 
bus_station 
## Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="black")




bar = opq(bbox = st_bbox(mnz)) %>% #Le pido que me traiga sobre el bbox de manzanas, pero nosotros debemos darle sobre "bogota colombia"
  add_osm_feature(key = "amenity", value = "bar") %>% #le digo que me devuelva los bares q estan cerca del polígono de manzanas, en puntos y sólo es osm_id
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
bar %>% head()



  
  
  
  
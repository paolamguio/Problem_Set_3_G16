
# Estadísticas descriptivas
# Problem_Set_3 
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
  
## preparación del espacio
rm(list = ls())

setwd("C:/Users/andre/Downloads")
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_3_G16/3. Stores")

## llamado librerías de la sesión
require(pacman)

p_load(tidyverse,rio,
       sf,
       leaflet,
       tmaptools,
       osmdata,
       nngeo)

## se importan bases de datos 
house_mnz <- import("house_mnz.rds")

house_mnz <- import("house_mnz2.rds")

###*** 1. Rescate de texto como variables ***###

## convierto el texto de description a minúscula
house_mnz$description <- str_to_lower(string = house_mnz$description)
house_mnz$description <- iconv(house_mnz$description, from = "UTF-8", to = "ASCII//TRANSLIT")
house_mnz$description <- str_replace_all(house_mnz$description, "[^[:alnum:]]", " ")
house_mnz$description <- gsub("\\s+", " ", str_trim(house_mnz$description))

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
a15 = "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m 2" 
a16 = "[:space:]+[:digit:]+[:space:]+m 2" 
a17 = "[:space:]+[:digit:]+m 2" 
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

# ajustes finales variable baños
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
d8 = "[:space:]+[:digit:]+[:space:]+garajes" 
d9 = "[:space:]+[:digit:]+garajes" 
d10 = "[:space:]+[:alpha:]+[:space:]+garajes"
d11 = "[:space:]+[:alpha:]+garajes" 
d12 = "garajes+[:space:]+[:digit:]" 
d13 = "garajes+[:space:]+[:alpha:]"
d14 = "garajes+[:digit:]" 
d15 = "garajes+[:alpha:]"
d16 = "[:space:]+[:digit:]+[:space:]+parqueadero" 
d17 = "[:space:]+[:digit:]+parqueadero" 
d18 = "[:space:]+[:alpha:]+[:space:]+parqueadero"
d19 = "[:space:]+[:alpha:]+parqueadero" 
d20 = "parqueadero+[:space:]+[:digit:]" 
d21 = "parqueadero+[:space:]+[:alpha:]"
d22 = "parqueadero+[:digit:]" 
d23 = "parqueadero+[:alpha:]"
d24 = "[:space:]+[:digit:]+[:space:]+parqueaderos" 
d25 = "[:space:]+[:digit:]+parqueaderos" 
d26 = "[:space:]+[:alpha:]+[:space:]+parqueaderos"
d27 = "[:space:]+[:alpha:]+parqueaderos" 
d28 = "parqueaderos+[:space:]+[:digit:]" 
d29 = "parqueaderos+[:space:]+[:alpha:]"
d30 = "parqueaderos+[:digit:]" 
d31 = "parqueaderos+[:alpha:]"
d32 = "[:space:]+[:digit:]+[:space:]+parqeadero" 
d33 = "[:space:]+[:digit:]+parqeadero" 
d34 = "[:space:]+[:alpha:]+[:space:]+parqeadero"
d35 = "[:space:]+[:alpha:]+parqeadero" 
d36 = "parqeadero+[:space:]+[:digit:]" 
d37 = "parqeadero+[:space:]+[:alpha:]"
d38 = "parqeadero+[:digit:]" 
d39 = "parqeadero+[:alpha:]"
d40 = "[:space:]+[:digit:]+[:space:]+parqeaderos" 
d41 = "[:space:]+[:digit:]+parqeaderos" 
d42 = "[:space:]+[:alpha:]+[:space:]+parqeaderos"
d43 = "[:space:]+[:alpha:]+parqeaderos" 
d44 = "parqeaderos+[:space:]+[:digit:]" 
d45 = "parqeaderos+[:space:]+[:alpha:]"
d46 = "parqeaderos+[:digit:]" 
d47 = "parqeaderos+[:alpha:]"

house_mnz = house_mnz %>%
  mutate(parqueadero = str_extract
         (string=house_mnz$description ,
           pattern = paste0(d, "|", d1, "|", d2, "|", d3, "|", d4, "|", d5, "|", d6, "|", d7, "|", d8, "|", d9, "|", d10, "|", d11, "|", d12, "|", d13, "|", d14, "|", d15, "|", d16, "|", d17, "|", d18, "|", d19, "|", d20, "|", d21, "|", d22, "|", d23, "|", d24, "|", d25, "|", d26, "|", d27, "|", d28, "|", d29, "|", d30, "|", d31, "|", d32, "|", d33, "|", d34, "|", d35, "|", d36, "|", d37, "|", d38, "|", d39, "|", d40, "|", d41, "|", d42, "|", d43, "|", d44, "|", d45, "|", d46, "|", d47)))

# ajustes finales variable parqueadero
house_mnz$parqueadero <- str_replace_all (string=house_mnz$parqueadero, "[:punct:]" , replacement = ".")
house_mnz$parqueadero <- str_replace_all (string=house_mnz$parqueadero, "[:alpha:]+[:digit:]" , replacement = "")
house_mnz$parqueadero <- str_replace_all (string=house_mnz$parqueadero, "[:alpha:]" , replacement = "")

house_mnz$parqueadero <- as.numeric(house_mnz$parqueadero)

#dummy parqueadero
house_mnz = house_mnz %>%
  mutate(parking = str_extract
         (string=house_mnz$description ,
           pattern = "parqueadero|garaje|parqeadero"))

house_mnz <- house_mnz %>% mutate(parking = ifelse(is.na(parking) == T, 0, 1))

table(house_mnz$parking)
table(is.na(house_mnz$parqueadero))

# Variable dummy de ascensor  
house_mnz = house_mnz %>%
  mutate(ascensor = str_extract
         (string=house_mnz$description ,
           pattern = "ascensor|asensor|acensor"))

house_mnz <- house_mnz %>% mutate(ascensor = ifelse(is.na(ascensor) == T, 0, 1))

table(house_mnz$ascensor)
table(is.na(house_mnz$ascensor))

house_mnz = house_mnz %>%
  mutate(balcon = str_extract
         (string=house_mnz$description ,
           pattern = "balcon|balcn"))

house_mnz <- house_mnz %>% mutate(balcon = ifelse(is.na(balcon) == T, 0, 1))

table(house_mnz$balcon)

house_mnz = house_mnz %>%
  mutate(terraza = str_extract
         (string=house_mnz$description ,
           pattern = "terraza"))

house_mnz <- house_mnz %>% mutate(terraza = ifelse(is.na(terraza) == T, 0, 1))

table(house_mnz$terraza)

house_mnz = house_mnz %>%
  mutate(remodelado = str_extract
         (string=house_mnz$description ,
           pattern = "remodelado"))

house_mnz <- house_mnz %>% mutate(remodelado = ifelse(is.na(remodelado) == T, 0, 1))

table(house_mnz$remodelado)

table(is.na(house_mnz$rooms))

table(is.na(house_mnz[is.na(house_mnz$rooms) == T,]$alcobas))

table(is.na(house_mnz$alcobas))

table(is.na(house_mnz$bedrooms))

table(is.na(house_mnz$bathrooms))

table(is.na(house_mnz[is.na(house_mnz$bathrooms) == T,]$baños))

table(is.na(house_mnz$surface_covered))

table(is.na(house_mnz[is.na(house_mnz$surface_covered) == T,]$area_total))


### *** Imputación de valores con datos espaciales de los vecinos ***###

## NA área total 
house_mnz = house_mnz %>%  
  mutate(surface_total = ifelse(is.na(surface_total)==T, 
                                area_total,surface_total)) 

# se imputan valores de los vecinos cercanos por mzn
house_mnz = house_mnz %>%
  group_by(COD_DANE_ANM) %>% 
  mutate(surface_total_med=median(surface_total,na.rm=T)) 

house_mnz = house_mnz %>%  
  mutate(surface_total = ifelse(is.na(surface_total)==T, 
                                surface_total_med,surface_total)) 

table(is.na(house_mnz$surface_total))

## NA baños
house_mnz = house_mnz %>%  
  mutate(bathrooms = ifelse(is.na(bathrooms)==T, 
                            baños,bathrooms)) 


table(is.na(house_mnz$bathrooms)) #teníamos 7324 NA

# se imputan valores de los vecinos cercanos por mzn
house_mnz = house_mnz %>%
  group_by(COD_DANE_ANM) %>% 
  mutate(bathrooms_med=median(bathrooms,na.rm=T)) 

house_mnz = house_mnz %>%  
  mutate(bathrooms = ifelse(is.na(bathrooms)==T, 
                            bathrooms_med,bathrooms)) 

table(is.na(house_mnz$bathrooms))

table(is.na(house_mnz$med_VA1_ESTRATO))

leaflet() %>% addTiles() %>% addPolygons(data=house_mnz[1,] %>% st_buffer(dist = 0.0005))

buffer <- st_buffer(house_mnz, dist = 0.0005)

leaflet() %>% addTiles() %>% addPolygons(data=buffer)

buffer = st_join(buffer,house_mnz[,"med_VA1_ESTRATO"])

st_geometry(buffer) = NULL

buffer_med = buffer %>% group_by(property_id) %>% summarise(estrato_med=median(med_VA1_ESTRATO.y,na.rm=T))

house_mnz = left_join(house_mnz,buffer_med,"property_id")

house_mnz = house_mnz %>%  
  mutate(estrato = ifelse(is.na(med_VA1_ESTRATO)==T, 
                          estrato_med,med_VA1_ESTRATO)) 

table(is.na(house_mnz$estrato))

house_mnz2 <- house_mnz %>% select(c("property_id", "bathrooms", "surface_total", "estrato"))

buffer2 <- st_buffer(house_mnz2, dist = 0.0005)

buffer2 = st_join(buffer2,house_mnz2[, c("bathrooms", "surface_total", "estrato")])

st_geometry(buffer2) = NULL

buffer_med2 = buffer2 %>% group_by(property_id) %>% summarise(estrato_med2=median(estrato.y,na.rm=T),
                                                              bathrooms_med2=median(bathrooms.y,na.rm=T),
                                                              surface_total_med2=median(surface_total.y,na.rm=T))

house_mnz = left_join(house_mnz,buffer_med2,"property_id")

house_mnz = house_mnz %>%  
  mutate(estrato = ifelse(is.na(estrato)==T, 
                          estrato_med2,estrato),
         bathrooms = ifelse(is.na(bathrooms)==T, 
                          bathrooms_med2,bathrooms),
         surface_total = ifelse(is.na(surface_total)==T, 
                            surface_total_med2,surface_total)) 

house_mnz3 <- house_mnz %>% select(c("property_id", "bathrooms", "surface_total", "estrato"))

buffer3 <- st_buffer(house_mnz3, dist = 0.001)

buffer3 = st_join(buffer3,house_mnz3[, c("bathrooms", "surface_total", "estrato")])

st_geometry(buffer3) = NULL

buffer_med3 = buffer3 %>% group_by(property_id) %>% summarise(estrato_med3=median(estrato.y,na.rm=T),
                                                              bathrooms_med3=median(bathrooms.y,na.rm=T),
                                                              surface_total_med3=median(surface_total.y,na.rm=T))

house_mnz = left_join(house_mnz,buffer_med3,"property_id")

house_mnz = house_mnz %>%  
  mutate(estrato = ifelse(is.na(estrato)==T, 
                          estrato_med3,estrato),
         bathrooms = ifelse(is.na(bathrooms)==T, 
                            bathrooms_med3,bathrooms),
         surface_total = ifelse(is.na(surface_total)==T, 
                                surface_total_med3,surface_total)) 

house_mnz4 <- house_mnz %>% select(c("property_id", "bathrooms", "surface_total", "estrato"))

buffer4 <- st_buffer(house_mnz4, dist = 0.002)

buffer4 = st_join(buffer4,house_mnz4[, c("bathrooms", "surface_total", "estrato")])

st_geometry(buffer4) = NULL

buffer_med4 = buffer4 %>% group_by(property_id) %>% summarise(estrato_med4=median(estrato.y,na.rm=T),
                                                              bathrooms_med4=median(bathrooms.y,na.rm=T),
                                                              surface_total_med4=median(surface_total.y,na.rm=T))

house_mnz = left_join(house_mnz,buffer_med4,"property_id")

house_mnz = house_mnz %>%  
  mutate(estrato = ifelse(is.na(estrato)==T, 
                          estrato_med4,estrato),
         bathrooms = ifelse(is.na(bathrooms)==T, 
                            bathrooms_med4,bathrooms),
         surface_total = ifelse(is.na(surface_total)==T, 
                                surface_total_med4,surface_total)) 

colnames(house_mnz)

house_mnz <- house_mnz %>% select(c("property_id", "ad_type", "l1", "l2", "l3", "bedrooms", "bathrooms", "surface_total", "price", "currency", "property_type", "base", "dist_bar", "dist_bus_station", "dist_bank", "dist_restaurant", "dist_school", "dist_park", "dist_parks_total", "Neighborhood", "parking", "ascensor", "balcon", "terraza", "remodelado", "estrato"))

table(house_mnz$base, is.na(house_mnz$bathrooms))
table(house_mnz$base, is.na(house_mnz$surface_total))
table(house_mnz$base, is.na(house_mnz$estrato))

house_mnz <- house_mnz %>% subset(is.na(estrato) == F)

table(house_mnz$base, is.na(house_mnz$estrato))

export(house_mnz,"df_house_mnz2.rds")

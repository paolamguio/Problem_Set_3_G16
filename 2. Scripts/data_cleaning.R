rm(list = ls())

setwd("C:/Users/andre/Downloads")

require(pacman)

p_load(tidyverse,rio,
       sf,
       leaflet,
       tmaptools,
       osmdata,
       nngeo)

train <- import("train.Rds") %>% mutate(base="train")
test <- import("test.Rds") %>% mutate(base="test")
house <- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)

leaflet() %>% addTiles() %>% addCircles(data=house)

str(house)

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

mnz = import("mnz_urban.rds")

sf_use_s2(FALSE)

mnz_chapinero <- mnz[chapinero,]

leaflet() %>% addTiles() %>% addCircles(data=house_chapinero) %>% addPolygons(data = mnz_chapinero, col = "red")

mnz_poblado <- mnz[poblado,]

leaflet() %>% addTiles() %>% addCircles(data=house_poblado) %>% addPolygons(data = mnz_poblado, col = "red")

house_chapinero_mnz = st_join(house_chapinero,mnz_chapinero)

colnames(house_chapinero_mnz)

table(is.na(house_chapinero_mnz$COD_DANE_ANM))

db_1 = house_chapinero_mnz %>% subset(is.na(COD_DANE_ANM)==F)

db_2 = house_chapinero_mnz %>% subset(is.na(COD_DANE_ANM)==T) %>% mutate(COD_DANE_ANM=NULL,
                                                                         med_H_NRO_CUARTOS=NULL,
                                                                         med_HA_TOT_PER=NULL,
                                                                         med_V_TOT_HOG=NULL,
                                                                         med_VA1_ESTRATO=NULL)

leaflet() %>% addTiles() %>% addPolygons(data=db_2[1,] %>% st_buffer(dist = 0.0005))

db_2 = st_join(st_buffer(db_2,dist = 0.0005) , mnz_chapinero) %>% 
  subset(duplicated(property_id)==F)

house_chapinero_mnz <- rbind(db_1, db_2)

table(is.na(house_chapinero_mnz$COD_DANE_ANM))

house_poblado_mnz = st_join(house_poblado,mnz_poblado)

colnames(house_poblado_mnz)

table(is.na(house_poblado_mnz$COD_DANE_ANM))

db_3 = house_poblado_mnz %>% subset(is.na(COD_DANE_ANM)==F)

db_4 = house_poblado_mnz %>% subset(is.na(COD_DANE_ANM)==T) %>% mutate(COD_DANE_ANM=NULL,
                                                                         med_H_NRO_CUARTOS=NULL,
                                                                         med_HA_TOT_PER=NULL,
                                                                         med_V_TOT_HOG=NULL,
                                                                         med_VA1_ESTRATO=NULL)

leaflet() %>% addTiles() %>% addPolygons(data=db_3[1,] %>% st_buffer(dist = 0.0005))

db_4 = st_join(st_buffer(db_4,dist = 0.0005) , mnz_poblado) %>% 
  subset(duplicated(property_id)==F)

house_poblado_mnz <- rbind(db_3, db_4)

table(is.na(house_poblado_mnz$COD_DANE_ANM))

leaflet() %>% addTiles() %>% addCircles(data=house) %>% addPolygons(data = mnz, col = "red")

bar_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_chapinero , col="red")

bar_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bar_poblado , col="red")

osm_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_chapinero <- osm_chapinero %>% osmdata_sf()

bus_station_chapinero <- osm_sf_chapinero$osm_points %>% select(osm_id,amenity)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_chapinero , col="red")

osm_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key="amenity" , value="bus_station")

osm_sf_poblado <- osm_poblado %>% osmdata_sf()

bus_station_poblado <- osm_sf_poblado$osm_points %>% select(osm_id,amenity)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station_poblado , col="red")

bank_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bank_chapinero , col="red")

bank_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "bank") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=bank_poblado , col="red")

restaurant_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant_chapinero , col="red")

restaurant_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurant_poblado , col="red")

school_chapinero <- opq(bbox = st_bbox(mnz_chapinero)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=school_chapinero , col="red")

school_poblado <- opq(bbox = st_bbox(mnz_poblado)) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)

leaflet() %>% addTiles() %>% addCircleMarkers(data=school_poblado , col="red")


# Chapinero - distancia al parque de la 93
parkch = getbb(place_name = "Parque de la 93", 
                 featuretype = "amenity",
                 format_out = "sf_polygon")
parkch %>% head()

leaflet() %>% addTiles() %>% addPolygons(data=parkch , col="green")

# Poblado - distancia a parque lleras
parkp = getbb(place_name = "Parque Lleras", 
              featuretype = "amenity",
              format_out = "sf_polygon")
parkp %>% head()

leaflet() %>% addTiles() %>% addPolygons(data=parkp , col="green")


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

house_chapinero_mnz$dist_park <- st_distance(x = house_chapinero_mnz, y = parkch)
house_poblado_mnz$dist_park <- st_distance(x = house_poblado_mnz, y = parkp)

house_chapinero_mnz <- house_chapinero_mnz %>% mutate(Neighborhood = "Chapinero")

house_poblado_mnz <- house_poblado_mnz %>% mutate(Neighborhood = "Poblado")

house_mnz <- rbind(house_chapinero_mnz, house_poblado_mnz)

export(house_mnz,"house_mnz.rds")
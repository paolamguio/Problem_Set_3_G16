
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

p_load(tidyverse,
  rvest,
  writexl,
  rio,
  skimr,
  sf,
  pastecs,
  stargazer,
  PerformanceAnalytics,
  naniar,
  gtsummary,
  stringr,
  rgeos, 
  plotly, 
  leaflet, 
  tmaptools,
  osmdata
)

##### Estadísticas descriptivas base train #####

## se importan bases de datos creada en 1.data_cleaning
df_hogares<- readRDS("df_house_mnz2.rds")
class(df_hogares)
dim(df_hogares)
colnames(df_hogares)

#ajustes en variable estrato
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "1.5" , replacement = "2")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "2.5" , replacement = "3")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "3.25" , replacement = "3")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "3.375" , replacement = "3")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "3.5" , replacement = "4")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "4.5" , replacement = "5")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "5.25" , replacement = "5")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "5.5" , replacement = "6")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "5.75" , replacement = "6")
df_hogares$estrato <- str_replace_all (string= df_hogares$estrato , pattern = "5.875" , replacement = "6")

df_hogares$estrato <- as.numeric(df_hogares$estrato)
table(df_hogares$estrato)

st_geometry(df_hogares) = NULL
class(df_hogares)

df_hogares <- df_hogares %>% mutate(base_Neighborhood = paste0(base, " ", Neighborhood))

table(df_hogares$base_Neighborhood)

table(is.na(df_hogares$Neighborhood))

# separación base de datos

df_trainCH <- df_hogares %>% subset(base == "train" & Neighborhood == "Chapinero") 
df_trainP <- df_hogares %>% subset(base == "train" & Neighborhood == "Poblado") 

df_testCH <- df_hogares %>% subset(base == "test" & Neighborhood == "Chapinero") 
df_testP <- df_hogares %>% subset(base == "test" & Neighborhood == "Poblado") 


df_train <- df_hogares %>% subset(base == "train") 


### 1. estadísticas descriptivas ###
summary(df_trainCH)
summary(df_trainP)
summary(df_testCH)
summary(df_testP)

# estadísiticas descriptivas generales datos

stat.desc(df_trainCH)
descriptivastch <- stat.desc(df_trainCH) # se guardan las estadísticas descriptivas de todas las variables para luego exportarlas a un excel
descriptivastch$Estadisticas <- row.names(descriptivastch) # se crea columna dentro del dataframe con el nombre de las filas 
descriptivastch <- descriptivastch %>% select(Estadisticas, everything()) # se ubica la columna creada en la primera posición 
write_xlsx(descriptivastch, "d_train_ch.xlsx") # se exporta a excel tabla con las estadísticas descriptivas

stat.desc(df_trainP)
descriptivastp <- stat.desc(df_trainP)
descriptivastp$Estadisticas <- row.names(descriptivastp) 
descriptivastp <- descriptivastp %>% select(Estadisticas, everything())
write_xlsx(descriptivastp, "d_train_p.xlsx")

stat.desc(df_testCH)
descriptivasch <- stat.desc(df_testCH) 
descriptivasch$Estadisticas <- row.names(descriptivasch) 
descriptivasch <- descriptivasch %>% select(Estadisticas, everything())  
write_xlsx(descriptivasch, "d_test_CH.xlsx") 

stat.desc(df_testP)
descriptivasp <- stat.desc(df_testP)
descriptivasp$Estadisticas <- row.names(descriptivasp)
descriptivasp <- descriptivasp %>% select(Estadisticas, everything())
write_xlsx(descriptivasp, "d_test_p.xlsx") 

# Tablas descriptivas

df_hogares1 <- df_hogares %>% select(bedrooms, bathrooms, surface_total, price, property_type, dist_bar, dist_bus_station, dist_bank, dist_restaurant, dist_school, dist_park, parking, ascensor, balcon, terraza, remodelado, estrato, base_Neighborhood)
table1 <- tbl_summary(df_hogares1)

table1

tbl_summary(df_hogares1, by= base_Neighborhood, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación

# Gráficos por localidad 
p <- ggplot(df_trainCH, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta CH", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

p1 <- ggplot(df_trainP, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta P", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p1)


#Relación entre distancia al parque más cercano y precio 
p2 <- ggplot(df_trainCH, aes(x = dist_park, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia al parque de la 93", 
       y = "Valor venta inmueble",
       title = "Relación entre distancia al parque de la 93 y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p2) 

p3 <- ggplot(df_trainP, aes(x = dist_park, y = price)) +
  geom_point(col = "darkblue", alpha = 0.4) +
  labs(x = "Distancia al parque Lleras", 
       y = "Valor venta inmueble",
       title = "Relación entre distancia al parque Lleras y el valor del inmueble") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p3)


#Gráficas entre chapinero y poblado - base train
p4 <- ggplot(df_train, aes(x = dist_park, y = price, color=Neighborhood)) +
  geom_point()
ggplotly(p4)


p5 <- ggplot(df_train, aes(x = estrato, y = price, color=Neighborhood)) +
  geom_point()+
  labs(x = "Estrato", 
       y = "Valor venta inmueble",
       title = "Relación entre estrato y valor del inmueble") +
   scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p5)
 
p6 <- ggplot(df_train, aes(x = dist_bus_station, y = price, color=Neighborhood)) +
  geom_point()+
  labs(x = "Distancia a estación de bus", 
       y = "Valor venta inmueble",
       title = "Relación distancia a servicio de transporte y valor del inmueble") +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p6)

p7 <- ggplot(df_train, aes(x = property_type, y = price, color=Neighborhood)) +
  geom_point()+
  labs(x = "Tipo de inmueble", 
       y = "Valor venta inmueble",
       title = "Relación tipo y valor del inmueble") +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p7)

p8 <- ggplot(df_train, aes(x = bedrooms, y = price, color=Neighborhood)) +
  geom_point()+
  labs(x = "No. Habitaciones", 
       y = "Valor venta inmueble",
       title = "Relación entre habitaciones y valor del inmueble") +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p8)


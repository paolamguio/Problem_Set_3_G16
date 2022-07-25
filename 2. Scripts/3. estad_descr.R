
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
  gtsummary
)

##### Estadísticas descriptivas base train #####

## se importan bases de datos creada en 1.data_cleaning
df_hogares<- readRDS("df_house_mnz.rds")

df_hogares<- readRDS("df_house_mnz2.rds")

class(df_hogares)
dim(df_hogares)
colnames(df_hogares)

st_geometry(df_hogares) = NULL

df_hogares <- df_hogares %>% mutate(base_Neighborhood = paste0(base, " ", Neighborhood))

table(df_hogares$base_Neighborhood)

df_hogares$estrato <- as.factor(df_hogares$estrato)

table(df_hogares$Neighborhood == "Poblado")

sum(df_hogares$Neighborhood)

table(is.na(df_hogares$Neighborhood))

summary(is.na(df_hogares$Neighborhood==T))
        
df_hogares$Neighborhood

# separación de base de datos

df_trainCH <- df_hogares %>% subset(base == "train" & Neighborhood == "Chapinero") 
df_trainP <- df_hogares %>% subset(base == "train" & Neighborhood == "Poblado") 

df_testCH <- df_hogares %>% subset(base == "test" & Neighborhood == "Chapinero") 
df_testP <- df_hogares %>% subset(base == "test" & Neighborhood == "Poblado") 

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
descriptivasch <- stat.desc(df_testCH) # se guardan las estadísticas descriptivas de todas las variables para luego exportarlas a un excel
descriptivasch$Estadisticas <- row.names(descriptivasch) # se crea columna dentro del dataframe con el nombre de las filas 
descriptivasch <- descriptivasch %>% select(Estadisticas, everything()) # se ubica la columna creada en la primera posición 
write_xlsx(descriptivasch, "d_test_CH.xlsx") # se exporta a excel tabla con las estadísticas descriptivas

stat.desc(df_testP)
descriptivasp <- stat.desc(df_testP) # se guardan las estadísticas descriptivas de todas las variables para luego exportarlas a un excel
descriptivasp$Estadisticas <- row.names(descriptivasp) # se crea columna dentro del dataframe con el nombre de las filas 
descriptivasp <- descriptivasp %>% select(Estadisticas, everything()) # se ubica la columna creada en la primera posición 
write_xlsx(descriptivasp, "d_test_p.xlsx") # se exporta a excel tabla con las estadísticas descriptivas







# Tablas descriptivas

df_hogares1 <- df_hogares %>% select(bedrooms, bathrooms, property_type, parking, ascensor, balcon, terraza, remodelado, estrato, base_Neighborhood)
table1 <- tbl_summary(df_hogares1)

table1

tbl_summary(df_hogares1, by= base_Neighborhood, statistic = list (all_continuous()~"{mean} ({sd})")) # por clasificación



# Inclusión de manzanas 
# Problem_Set_3 
# Grupo 16
# Andres Martinez, Paola Morales y Oscar Cortes 
--------------------------------------------------
 
## preparación del espacio
rm(list = ls())
setwd("C:/Users/amorales/OneDrive - ANI/Documentos/GitHub/Problem_Set_3-G16/3. Stores")
setwd("C:/Users/andre/Downloads")

## llamado librerías de la sesión
require(pacman)
p_load(rio,tidyverse,sf)

###*** 1. Rescate de texto como variables ***###

## censo data
browseURL("https://microdatos.dane.gov.co//catalog/643/get_microdata")

## load data
mgn = import("CNPV2018_MGN_A2_11.CSV")
colnames(mgn)
distinct_all(mgn[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

hog = import("CNPV2018_2HOG_A2_11.CSV")
colnames(hog)
distinct_all(hog[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA","H_NROHOG")]) %>% nrow()

viv = import("CNPV2018_1VIV_A2_11.CSV") 
colnames(viv)
distinct_all(viv[,c("COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

## join data
viv_hog = left_join(hog,viv,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
table(is.na(viv_hog$VA1_ESTRATO))

data = left_join(viv_hog,mgn,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
table(is.na(data$VA1_ESTRATO))

## select vars
H_NRO_CUARTOS = "Número de cuartos en total"
HA_TOT_PER = "Total personas en el hogar"
V_TOT_HOG = "Total de hogares en la vivienda"
VA1_ESTRATO = "Estrato de la vivienda (según servicio de energía)"
COD_DANE_ANM = "Codigo DANE de manzana"

db = data %>% select(COD_DANE_ANM,H_NRO_CUARTOS,HA_TOT_PER,V_TOT_HOG,VA1_ESTRATO)

## summary data
df = db %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
            med_HA_TOT_PER=median(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data
export(df,"mnz_censo.rds")

mgn2 = import("CNPV2018_MGN_A2_05.CSV")
colnames(mgn2)
distinct_all(mgn2[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

hog2 = import("CNPV2018_2HOG_A2_05.CSV")
colnames(hog2)
distinct_all(hog2[,c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA","H_NROHOG")]) %>% nrow()

viv2 = import("CNPV2018_1VIV_A2_05.CSV") 
colnames(viv2)
distinct_all(viv2[,c("COD_ENCUESTAS","U_VIVIENDA")]) %>% nrow()

## join data
viv_hog2 = left_join(hog2,viv2,by=c("COD_ENCUESTAS","U_VIVIENDA","UA_CLASE"))
table(is.na(viv_hog2$VA1_ESTRATO))

data2 = left_join(viv_hog2,mgn2,by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
table(is.na(data2$VA1_ESTRATO))

## select vars
H_NRO_CUARTOS = "Número de cuartos en total"
HA_TOT_PER = "Total personas en el hogar"
V_TOT_HOG = "Total de hogares en la vivienda"
VA1_ESTRATO = "Estrato de la vivienda (según servicio de energía)"
COD_DANE_ANM = "Codigo DANE de manzana"

db2 = data2 %>% select(COD_DANE_ANM,H_NRO_CUARTOS,HA_TOT_PER,V_TOT_HOG,VA1_ESTRATO)

## summary data
df2 = db2 %>%
  group_by(COD_DANE_ANM) %>% 
  summarise(med_H_NRO_CUARTOS=median(H_NRO_CUARTOS,na.rm=T), 
            med_HA_TOT_PER=median(HA_TOT_PER,na.rm=T), 
            med_V_TOT_HOG=median(V_TOT_HOG,na.rm=T),
            med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))

## export data
export(df2,"mnz_censo2.rds")

mnz <- rbind(df, df2)

export(mnz,"mnz.rds")

mnz_urban <- st_read("11_Bogota/URBANO/MGN_URB_MANZANA.shp")

mnz_urban2 <- st_read("05_Antioquia/URBANO/MGN_URB_MANZANA.shp")

colnames(mnz_urban)
colnames(mnz_urban2)
colnames(mnz)

mnz_urban <- mnz_urban %>% mutate(COD_DANE_ANM = MANZ_CCNCT)

mnz_urban2 <- mnz_urban2 %>% mutate(COD_DANE_ANM = MANZ_CCNCT)

mnz_urban <- mnz_urban %>% select(c("COD_DANE_ANM", "geometry"))

mnz_urban2 <- mnz_urban2 %>% select(c("COD_DANE_ANM", "geometry"))

mnz_urban <- rbind(mnz_urban, mnz_urban2)

mnz_urban <- left_join(mnz_urban, mnz)

mnz_urban

export(mnz_urban,"mnz_urban.rds")
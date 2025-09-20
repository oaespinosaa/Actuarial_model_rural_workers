# Librerias y limpiar entorno de trabajo
library(tidyverse)
library(readxl)
rm(list = ls());gc()

# --------------------------- Bases de datos -----------------------------------
# DANE - Población municipal - DNP rural
popRural <- readRDS('Datos/popUR.rds') %>%
  filter(region == 'Rural') %>%
  group_by(year,edad,sexo) %>% summarise(popR = sum(pop),.groups = 'drop') %>%
  mutate(Sexo = ifelse(sexo == 'Femenino','FEMENINO','MASCULINO')) %>% select(-sexo)
# DANE - GEIH último año finalizado dic 2023
GEIH <- readRDS('Datos/GEIH_2023.rds') %>%
  mutate(P6040 = as.numeric(P6040),Sexo = ifelse(P6020 == 2,'FEMENINO','MASCULINO'))
# DNP - Sisbén IV 2023
sisbenR <- readRDS('Datos/SISBEN_Rural.rds') %>%
  mutate(Sexo = ifelse(PER001==1,'MASCULINO','FEMENINO'))
# C035 - Clase de riesgo CIIU V3
cr_geih3 <- read_excel('Datos/c035_anio_cr_ciiu3_geih_2017_082022.xlsx') %>%
  filter(NRO_AHNO != 2022) %>%
  bind_rows(read_excel('Datos/c035_cl_2022_2023_CIIU3.xlsx')) %>%
  group_by(NRO_AHNO) %>% mutate(prop = personas_unicas/sum(personas_unicas)) %>%
  ungroup

################################################################################
# ------------------------ | Modelo 1: Dignidad rural | ------------------------
# DANE - Proyecciones de población 2024-2028 DNP rural
popEdadSexo <- bind_rows(
  popRural %>% filter(edad >= 15,edad <= 59,Sexo == 'FEMENINO'),
  popRural %>% filter(edad >= 15,edad <= 64,Sexo == 'MASCULINO')
) %>% group_by(year,edad,Sexo) %>% summarise(popR = sum(popR),.groups = 'drop') %>%
  filter(year %in% c(2024:2028))
popEdadSexo

# GEIH - Aproximación de expuestos
# Diccionario: "Datos/ddi-documentation-spanish-782.pdf"
# CLASE: 2 rural, IE: 1 informal, P6100: 3 subsidiado, P6990: ARL No/NR
# P6040: edad, P6020(P3271): Sexo 1 Hombre 2 Mujer
# Escenario trabajadores informales (RS, sin ARL)
popGEIHEdadSexo <- bind_rows(
  GEIH %>% filter(CLASE == 2,P6040 >= 15,P6040 <= 59,Sexo == 'FEMENINO'),
  GEIH %>% filter(CLASE == 2,P6040 >= 15,P6040 <= 64,Sexo == 'MASCULINO')) %>%
  filter(baseOcu == 1,IE == 1,P6100 == 3,P6990 != 1) %>%
  group_by(year,P6040,Sexo) %>%
  summarise(pop_ie = sum(FEX_C),.groups = "drop") %>%
  left_join(bind_rows(GEIH %>% filter(CLASE == 2,P6040 >= 15,P6040 <= 59,Sexo == 'FEMENINO'),
                      GEIH %>% filter(CLASE == 2,P6040 >= 15,P6040 <= 64,Sexo == 'MASCULINO')) %>%
              group_by(year,P6040,Sexo) %>%
              summarise(popTot = sum(FEX_C),.groups = "drop"),by = c('year','Sexo','P6040')) %>%
  mutate(propGEIH = pop_ie/popTot) %>% rename(edad = P6040)
popGEIHEdadSexo

# Sisbén IV, DNP rural
# Edad (per002)
## 3	12 a 17 años - 1/6
## 4	18 a 28 años - 1/11
## 5	29 a 59 años - 1/31
## 6	60 años y más - 1/15 (esperanza de vida 74)
aux_sisbenedad <- tibble(PER002 = c(3:6),
                         prop = c(1/(17-12+1),1/(28-18+1),1/(59-29+1),1/(74-60+1))) %>%
  left_join(tibble(edad = 12:74,PER002 = c(rep(3,17-12+1),rep(4,28-18+1),rep(5,59-29+1),rep(6,15))))
# Sexo (per001)
## 1	Hombre
## 2	Mujer
# Seguridad social (per007)
## 0	Ninguna
## 1	Contributivo
## 2	Especial (Fuerzas Armadas, Ecopetrol, universidades públicas, magisterio)
## 3	Subsidiado (EPS-S)
## 9	No sabe
# Privación IPM Proxy - Trabajo informal (i8)
## 0	No
## 1	Si
# Sisbén IV (grupos A y B)
sisbenF <- sisbenR %>% filter(PER002 >= 3,I8 == 1,PER007 == 3) %>%
  group_by(Sexo,PER002) %>% summarise(popS = sum(FEX),.groups = 'drop') %>%
  left_join(aux_sisbenedad,relationship = 'many-to-many') %>%
  mutate(popSF = popS*prop) %>% select(Sexo,edad,popSF)
sisbenFC <- sisbenR %>% filter(Grupo %in% c('A','B'),PER002 >= 3,I8 == 1,PER007 == 3) %>%
  group_by(Sexo,PER002) %>% summarise(popSAB = sum(FEX),.groups = 'drop') %>%
  left_join(aux_sisbenedad,relationship = 'many-to-many') %>%
  mutate(popSABF = popSAB*prop) %>% select(Sexo,edad,popSABF)

popSISBENEdadSexoAB <-
  bind_rows(sisbenFC %>% filter(edad >= 15,edad <= 59,Sexo == 'FEMENINO'),
            sisbenFC %>% filter(edad >= 15,edad <= 64,Sexo == 'MASCULINO'))%>%
  group_by(Sexo,edad) %>% summarise(popS = sum(popSABF),.groups = 'drop') %>%
  left_join(
    bind_rows(sisbenF %>% filter(edad >= 15,edad <= 59,Sexo == 'FEMENINO'),
              sisbenF %>% filter(edad >= 15,edad <= 64,Sexo == 'MASCULINO')) %>%
      group_by(Sexo,edad) %>% summarise(poptotS = sum(popSF),.groups = 'drop')
  ) %>%
  mutate(propSISBEN = popS/poptotS)
popSISBENEdadSexoAB

# Población cubierta por seguro de vida BEPS
polizabeps <- tibble(Sexo = rep(c('FEMENINO','MASCULINO'),each = 6),
                     year = rep(c(2016:2021),times = 2),
                     valor = c(12125,30406,65195,118647,109997,127249,
                               6626,15661,31333,58577,54133,64658))

seguroBEPSSexo <- tibble(Sexo = c('FEMENINO','MASCULINO'),
                         ahorrarR = c(137916,90619)) %>%
  left_join(tibble(Sexo = c('FEMENINO','MASCULINO'),
                   ahorrar_nac = c(515736,258386)),by = 'Sexo') %>%
  left_join(polizabeps %>% filter(year == 2021) %>% select(-year,Sexo,poliza_nac = valor),by = 'Sexo') %>%
  mutate(polizaR = poliza_nac*(ahorrarR/ahorrar_nac))

# Filtro seguro de vida BEPS
GEIH2021 <- readRDS('Datos/GEIH_2021.rds') %>%
  mutate(P6040 = as.numeric(P6040),Sexo = ifelse(P6020 == 2,'FEMENINO','MASCULINO'))
popGEIH2021 <- bind_rows(
  GEIH2021 %>% filter(CLASE == 2,P6040 >= 18,P6040 <= 59,Sexo == 'FEMENINO'),
  GEIH2021 %>% filter(CLASE == 2,P6040 >= 18,P6040 <= 64,Sexo == 'MASCULINO')) %>%
  filter(baseOcu == 1,IE == 1,P6100 == 3,P6990 != 1) %>%
  group_by(year,P6040,Sexo) %>%
  summarise(pop_ie2021 = sum(FEX_C),.groups = "drop") %>%
  left_join(bind_rows(GEIH2021 %>% filter(CLASE == 2,P6040 >= 18,P6040 <= 59,Sexo == 'FEMENINO'),
                      GEIH2021 %>% filter(CLASE == 2,P6040 >= 18,P6040 <= 64,Sexo == 'MASCULINO')) %>%
              group_by(year,P6040,Sexo) %>%
              summarise(popTot2021 = sum(FEX_C),.groups = "drop"),by = c('year','Sexo','P6040')) %>%
  mutate(propGEIH2021 = pop_ie2021/popTot2021) %>% rename(edad = P6040) %>%
  group_by(Sexo) %>% mutate(propGEIH2021 = propGEIH2021/sum(propGEIH2021)) %>%
  ungroup
popGEIH2021

popBEPSEdadSexo <- seguroBEPSSexo %>%
  left_join(popGEIH2021 %>% select(-year),by = 'Sexo') %>%
  left_join(popSISBENEdadSexoAB,by = c('Sexo','edad')) %>%
  mutate(polizaBEPS = polizaR*propGEIH2021*propSISBEN) %>%
  select(-propSISBEN,-popS,-poptotS)
popBEPSEdadSexo

# Población expuesta al riesgo 2024-2028:
# Proyección de población (2024 a 2028)* proporción GEIH (2023)*
# proporción Sisben IV (2023) - Seguro de vida BEPS
popEREdadSexo <- popEdadSexo %>% arrange(year) %>%
  left_join(popGEIHEdadSexo %>% select(-year), by = c('Sexo','edad')) %>%
  left_join(popSISBENEdadSexoAB,by = c('Sexo','edad')) %>%
  left_join(popBEPSEdadSexo,by = c('Sexo','edad')) %>%
  mutate(propT = propGEIH*propSISBEN,
         pop1 = round(popR*propGEIH*propSISBEN),
         popER = round(popR*propGEIH*propSISBEN)-round(ifelse(is.na(polizaBEPS),0,polizaBEPS)))
popEREdadSexo %>% group_by(year,Sexo) %>%
  summarise(popER = sum(popER),.groups = 'drop') %>%
  pivot_wider(names_from = Sexo,values_from = popER)
popEREdadSexo %>% group_by(year) %>% summarise(sum(popER))

################################################################################
# ------------------------ | Modelo 2: Subsidio rural | ------------------------
# DANE rural >= 18
pop2 <- popRural %>% filter(edad >= 18) %>% group_by(year) %>%
  summarise(popR = sum(popR),.groups = 'drop') %>%
  filter(year %in% c(2024:2028))
pop2

# Sisbén C
popSISBENC <- sisbenR %>% filter(Grupo %in% c('C'),PER002 %in% 4:6,I8 == 1,PER007 == 3) %>%
  summarise(popS = sum(FEX),.groups = 'drop') %>%
  bind_cols(sisbenR %>% filter(PER002 %in% 4:6,I8 == 1,PER007 == 3) %>%
              summarise(poptotS = sum(FEX),.groups = 'drop')) %>%
  mutate(propSISBENC = popS/poptotS)
popSISBENC

# GEIH rural >= 18, ocupados informales RS sin ARL
geih_er <- GEIH %>% filter(CLASE == 2,P6040 >= 18) %>%
  filter(baseOcu == 1,IE == 1,P6100 == 3,P6990 != 1)
# C035 clase de riesgo promedio 2017-2023
cr_geih3P <- cr_geih3 %>% filter(!is.na(COD_CLASE_RIESGO_AECT)) %>%
  group_by(COD_CLASE_RIESGO_AECT) %>% summarise(prop = mean(prop,na.rm = T),.groups = 'drop') %>%
  mutate(prop = prop/sum(prop))
# sum(cr_geih3P$prop)
# GEIH por clase de riesgo
popGEIHCR <- geih_er %>% group_by(year) %>%
  summarise(pop_ie = sum(FEX_C),key = 1,.groups = "drop") %>%
  left_join(cr_geih3P %>% mutate(key = 1),by= 'key') %>%
  mutate(n = pop_ie*prop) %>%
  left_join(GEIH %>% filter(CLASE == 2,P6040 >= 18) %>% group_by(year) %>%
              summarise(popTot = sum(FEX_C),.groups = "drop"),by = c('year')) %>%
  mutate(propGEIH = n/popTot)
popGEIHCR

# Población expuesta al riesgo 2024-2028:
# Proyección de población (2024 a 2028)* proporción GEIH CIIU (2023)*
# proporción Sisbén IV C (2023)
popER2 <- pop2 %>% arrange(year) %>% mutate(key = 1) %>%
  left_join(popGEIHCR %>% select(-year) %>% mutate(key = 1), by = 'key',
            relationship = 'many-to-many') %>%
  left_join(popSISBENC %>% mutate(key = 1),by = 'key',
            relationship = 'many-to-many') %>%
  mutate(propT = propGEIH*propSISBENC,
         popER = round(popR*propGEIH*propSISBENC))
popER2 %>% select(year,COD_CLASE_RIESGO_AECT,popER) %>%
  pivot_wider(names_from = COD_CLASE_RIESGO_AECT,values_from = popER)
popER2 %>% group_by(year) %>% summarise(sum(popER))

################################################################################
# ---------------------- | Modelo 3: Identificar evasores | --------------------
# DANE rural >= 18 por sexo
pop3 <- popRural %>% filter(edad >= 18) %>% group_by(year,Sexo) %>%
  summarise(popR = sum(popR),.groups = 'drop') %>%
  filter(year %in% c(2024:2028))
pop3

# GEIH rural >= 18, ocupadas
geih_2 <- GEIH %>% filter(CLASE == 2,P6040 >= 18,baseOcu == 1)
# Ingresos >= 1 SMMLV, sin ARL, sin subsidios
smmlv2023 <- 1.16e6
popGEIHeva <- geih_2 %>% filter(!is.na(P6500),P6500 >= smmlv2023,P6990 != 1,
                                P6585S1 != 1,P6585S2 != 1,P6585S3 != 1,P6585S4 != 1) %>%
  group_by(year,Sexo) %>% summarise(pop = sum(FEX_C),.groups = 'drop') %>%
  left_join(GEIH %>% filter(CLASE == 2,P6040 >= 18) %>% group_by(year,Sexo) %>%
              summarise(popTot = sum(FEX_C),.groups = "drop"),by = c('year','Sexo')) %>%
  mutate(propGEIH = pop/popTot)

popER3 <- pop3 %>% arrange(year) %>%
  left_join(popGEIHeva %>% select(-year), by = 'Sexo') %>%
  mutate(popER = round(popR*propGEIH))
popER3 %>% select(year,Sexo,popER) %>%
  pivot_wider(names_from = Sexo,values_from = popER)
popER3 %>% group_by(year) %>% summarise(sum(popER))

################################################################################
# Exportar expuestos al riesgo
# writexl::write_xlsx(list(Modelo1 = popEREdadSexo,
#                          Modelo2 = popER2,
#                          Modelo3 = popER3),
#                     'Datos/ExpuestoRiesgo.xlsx')

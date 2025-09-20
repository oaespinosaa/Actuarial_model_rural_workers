# Librerías y limpiar entorno de trabajo
library(tidyverse)
library(readxl)
rm(list = ls());gc()

# DNP - Categorías de ruralidad
UR <- read.csv("Data/Categorias_de_Ruralidad.csv",encoding = "UTF-8",
               sep = ";") %>%
  mutate(region = ifelse(Categoria_ %in% c("Ciudades y aglomeraciones","Intermedio"),
                         "Urbano","Rural"),
         CODMUNI = ifelse(nchar(Municipaly) == 5,Municipaly,
                          formatC(Municipaly,width = 5,flag = "0"))) %>%
  select(CODMUNI,Municipali,region)

# ----------------------------- | ECV 2023 | ------------------------------ ####
ecv <- read.csv2('Data/ECV2023/Salud.CSV') %>%
  left_join(read.csv2('Data/ECV2023/Características y composición del hogar.CSV'),by = c('DIRECTORIO','SECUENCIA_P','ORDEN','FEX_C')) %>%
  left_join(read.csv2('Data/ECV2023/Datos de la vivienda.CSV'),by = c('DIRECTORIO','FEX_C')) %>%
  mutate(FEX_C = as.numeric(FEX_C),Sexo = ifelse(P6020==2,'FEMENINO','MASCULINO'),
         CODMUNI = paste0(formatC(P1_DEPARTAMENTO,width = 2,flag = "0"),
                          formatC(P1_MUNICIPIO,width = 3,flag = "0"))) %>%
  left_join(UR,by = 'CODMUNI') %>% rowwise() %>%
  mutate(disc = ifelse(any(P1906S1%in%c(1,2),P1906S2%in%c(1,2),P1906S3%in%c(1,2),
                           P1906S4%in%c(1,2),P1906S5%in%c(1,2),P1906S6%in%c(1,2),
                           P1906S7%in%c(1,2),P1906S8%in%c(1,2)),1,0)) %>% ungroup

########################### | Proxy días de incapacidad | ######################
# P431: Por ese problema de salud, ¿durante cuántos días en total dejó ____ de realizar sus actividades normales?
ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
  bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
  filter(P6100 %in% c(1,3)) %>% group_by(Sexo,P6100) %>%
  mutate(tot = sum(FEX_C)) %>% group_by(Sexo,P6100) %>%
  summarise(prom = mean(P6134,na.rm = T),
            prom_pond = sum(P6134*FEX_C/tot,na.rm = T),
            nas = sum(is.na(P6134)),.groups = 'drop') %>%
  mutate(P6100 = ifelse(P6100==1,'rc','rs'))

ecv_RCd <- ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
  bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
  filter(P6100 == 1) %>% group_by(Sexo) %>% mutate(tot = sum(FEX_C)) %>%
  summarise(rc = sum(P6134*FEX_C/tot,na.rm = T),.groups = 'drop')
ecv_RSd <- ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
  bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
  filter(P6100 == 3) %>% group_by(Sexo) %>% mutate(tot = sum(FEX_C)) %>%
  summarise(rs = sum(P6134*FEX_C/tot,na.rm = T),.groups = 'drop')
incap <- ecv_RCd %>% left_join(ecv_RSd) %>% mutate(prop = rs/rc) %>%
  left_join(ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
              bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
              group_by(Sexo) %>% summarise(pop = sum(FEX_C),.groups ='drop')
  ) %>% ungroup %>% mutate(tot = sum(pop)) %>%
  summarise(propt = sum(prop*pop/tot)) %>% pull(propt)

########################### | Proxy incapacidad permanente | ###################
# P1906: Dada su condición física y mental, en su vida diaria ______ tiene dificultades para realizar las siguientes actividades:
ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
  bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
  filter(P6100 %in% c(1,3)) %>% group_by(Sexo,P6100,disc) %>%
  summarise(conteo = n(),
            expandido = sum(FEX_C,na.rm = T),.groups = 'drop') %>%
  mutate(P6100 = ifelse(P6100==1,'rc','rs'))

ecv_RCi <- ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
  bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
  filter(P6100 == 1) %>% group_by(Sexo,disc) %>% summarise(n = sum(FEX_C),.groups = 'drop') %>%
  group_by(Sexo) %>% mutate(rc = n/sum(n)) %>% ungroup
ecv_RSi <- ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
  bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
  filter(P6100 == 3) %>% group_by(Sexo,disc) %>% summarise(n = sum(FEX_C),.groups = 'drop') %>%
  group_by(Sexo) %>% mutate(rs = n/sum(n)) %>% ungroup
perma <- ecv_RCi %>% left_join(ecv_RSi,by = c('disc','Sexo')) %>% mutate(prop = rs/rc) %>%
  left_join(ecv %>% filter(region == 'Rural',Sexo == 'FEMENINO',P6040 >= 15,P6040 <= 59) %>%
              bind_rows(ecv %>% filter(region == 'Rural',Sexo == 'MASCULINO',P6040 >= 15,P6040 <= 64)) %>%
              group_by(Sexo) %>% summarise(pop = sum(FEX_C),.groups ='drop')
  ) %>% filter(disc == 1) %>% mutate(tot = sum(pop)) %>%
  summarise(propt = sum(prop*pop/tot)) %>% pull(propt)

# writexl::write_xlsx(tibble(tipo = c('Parcial','Permanente'),
#                            factor = c(incap,perma)),
#                     'Data/Factor_ajuste.xlsx')

# Libraries and clear workspace
library(tidyverse)
library(readxl)
rm(list = ls());gc()

# ---------------------------- Inputs ------------------------------------------
# Exposed to risk
ER1 <- read_excel('Data/RiskExposed.xlsx',sheet = 'Modelo1') %>%
  select(year,Sexo,edad,popER)
ER2 <- read_excel('Data/RiskExposed.xlsx',sheet = 'Modelo2') %>%
  select(year,CR=COD_CLASE_RIESGO_AECT,popER) %>%
  mutate(CR = as.numeric(CR))

# Model 1
incapacidad <- read_excel('Data/Inputs model 1.xlsx',sheet = 'Incapacidad_parcial') %>%
  mutate(Sexo = ifelse(SEXO == 'F','FEMENINO','MASCULINO'),
         tipo = 'Parcial') %>%
  select(tipo,Sexo,prop_diasidem_sini_rural,prop_sini_exp_rural)
permanente <- read_excel('Data/Inputs model 1.xlsx',sheet = 'Incapacidad_permanente') %>%
  mutate(Sexo = ifelse(SEXO == 'F','FEMENINO','MASCULINO'),
         tipo = 'Permanente') %>%
  select(tipo,Sexo,prop_sini_exp_rural)
tablamort <- bind_rows(read_excel('Data/Inputs model 1.xlsx',sheet = 'tabla_mortalidad_femenino') %>%
                         mutate(Sexo = 'FEMENINO'),
                       read_excel('Data/Inputs model 1.xlsx',sheet = 'tabla_mortalidad_masculino') %>%
                         mutate(Sexo = 'MASCULINO'))
indemnizacion <- tibble(AuxilioFunerario=5,Permanente=9,SeguroVida=12)
factores <- read_excel('Data/Adjust_factor.xlsx')

# DANE - Poverty monetary line 2021-2023
pobreza <- read_excel("Data/PobrezaMonetaria.xlsx") %>%
  mutate(prop = Pobreza/SMMLV)

# Model 2
cotARL <- read_excel('Data/CotizacionARL.xlsx') %>%
  rename(prop_cot = Valor)

# SMMLV 2024
smmlv2024 <- 1.3e6
inf2024 <- 0.0512
auminf <- 0.0387
aumprod <- 0.0343
# Inflacion
inflacion <- tibble(year = 2024:2028,
                    smmlv = c(smmlv2024,
                              smmlv2024*(1+inf2024),
                              smmlv2024*(1+inf2024)*(1+auminf),
                              smmlv2024*(1+inf2024)*(1+auminf)^2,
                              smmlv2024*(1+inf2024)*(1+auminf)^3))
# SMMLV growth
crecSMMLV <- tibble(year = 2024:2028,
                    smmlv = c(smmlv2024,
                              smmlv2024*(1+inf2024+aumprod),
                              smmlv2024*(1+inf2024+aumprod)*(1+auminf+aumprod),
                              smmlv2024*(1+inf2024+aumprod)*(1+auminf+aumprod)^2,
                              smmlv2024*(1+inf2024+aumprod)*(1+auminf+aumprod)^3))

# Scale value
scl <- 1e6 # millions COP

################################################################################
# -------------------- | Model 1. Dignidad rural | ------------------------ ####
# Monetary values
montoM1 <- pobreza %>% summarise(media = mean(prop),key = 1) %>%
  left_join(inflacion %>% mutate(key = 1),by = 'key') %>%
  mutate(indem = round(media*2,4),
         valor_incap = indem*smmlv,
         valor_smmlv = smmlv) %>%
  select(year,valor_incap,valor_smmlv)
montoM1

# Life insurance and assistance
muerte_auxseg <- ER1 %>% left_join(tablamort,by = c('Sexo','edad' = 'x')) %>%
  mutate(nro = popER*`q(x)`) %>%
  bind_cols(indemnizacion) %>%
  left_join(montoM1,by ='year') %>%
  mutate(costo_aux = valor_smmlv*AuxilioFunerario*nro/scl,
         costo_seg = valor_smmlv*SeguroVida*nro/scl) %>%
  group_by(year) %>% summarise(muertes = sum(nro),costo_aux = sum(costo_aux),
                               costo_seg = sum(costo_seg))

# Base case
# Temporary incapacity
indem_inc <- ER1 %>% group_by(year,Sexo) %>%
  summarise(popER = sum(popER),.groups = 'drop') %>%
  left_join(incapacidad,by = 'Sexo') %>%
  left_join(factores,by = 'tipo') %>%
  mutate(nro = popER*prop_sini_exp_rural*factor,
         mes_nro = nro*prop_diasidem_sini_rural/30) %>%
  left_join(montoM1,by = 'year') %>%
  mutate(costo = valor_incap*mes_nro/scl) %>%
  group_by(year) %>% summarise(inc = sum(nro),
                               mes_inc = sum(mes_nro),costo_inc = sum(costo))
# Permanent disability
indem_per <- ER1 %>% group_by(year,Sexo) %>%
  summarise(popER = sum(popER),.groups = 'drop') %>%
  left_join(permanente,by = 'Sexo') %>%
  left_join(factores,by = 'tipo') %>%
  mutate(nro = popER*prop_sini_exp_rural*factor) %>%
  left_join(montoM1,by ='year') %>%
  bind_cols(indemnizacion) %>%
  mutate(costo_per = valor_smmlv*Permanente*nro/scl) %>%
  group_by(year) %>% summarise(per = sum(nro),costo_per = sum(costo_per))
M1_Base <- indem_inc %>% left_join(indem_per,by = 'year') %>%
  left_join(muerte_auxseg,by = 'year') %>% rowwise %>%
  mutate(total = sum(costo_inc,costo_per,costo_aux,costo_seg),
         personas = sum(inc,per,muertes)) %>%
  ungroup %>% left_join(ER1 %>% group_by(year) %>%
                          summarise(popER = sum(popER),.groups = 'drop'),
                        by = 'year') %>%
  mutate(PP = total*scl/popER,ER_M1Base = popER,PC_M1Base = PP/(1-0.21),
         totalM1Base = PC_M1Base*popER/scl)

# Optimistic case -  decline 2% anual accident rate
# Temporary incapacity
indem_inc_Opt <- ER1 %>% group_by(year,Sexo) %>%
  summarise(popER = sum(popER),.groups = 'drop') %>%
  left_join(incapacidad,by = 'Sexo') %>%
  left_join(factores,by = 'tipo') %>%
  mutate(nro = popER*prop_sini_exp_rural*(1-0.02)^{year-2023}*factor,
         mes_nro = nro*prop_diasidem_sini_rural/30) %>%
  left_join(montoM1,by ='year') %>%
  mutate(costo = valor_incap*mes_nro/scl) %>%
  group_by(year) %>% summarise(inc = sum(nro),mes_inc = sum(mes_nro),
                               costo_inc = sum(costo))
# Permanent disability
indem_per_Opt <- ER1 %>% group_by(year,Sexo) %>%
  summarise(popER = sum(popER),.groups = 'drop') %>%
  left_join(permanente,by = 'Sexo') %>%
  left_join(factores,by = 'tipo') %>%
  mutate(nro = popER*prop_sini_exp_rural*(1-0.02)^{year-2023}*factor) %>%
  left_join(montoM1,by ='year') %>%
  bind_cols(indemnizacion) %>%
  mutate(costo_per = valor_smmlv*Permanente*nro/scl) %>%
  group_by(year) %>% summarise(per = sum(nro),costo_per = sum(costo_per))
M1_Opt <- indem_inc_Opt %>% left_join(indem_per_Opt,by = 'year') %>%
  left_join(muerte_auxseg,by = 'year') %>% rowwise %>%
  mutate(total = sum(costo_inc,costo_per,costo_aux,costo_seg),
         personas = sum(inc,per,muertes)) %>%
  ungroup %>% left_join(ER1 %>% group_by(year) %>%
                          summarise(popER = sum(popER),.groups = 'drop'),
                        by = 'year') %>%
  mutate(PP = total*scl/popER,ER_M1Opt = popER,PC_M1Opt = PP/(1-0.21),
         totalM1Opt = PC_M1Opt*popER/scl)

# Pessimistic case - rise 2% anual accident rate
# Temporary incapacity
indem_inc_Pes <- ER1 %>% group_by(year,Sexo) %>%
  summarise(popER = sum(popER),.groups = 'drop') %>%
  left_join(incapacidad,by = 'Sexo') %>%
  left_join(factores,by = 'tipo') %>%
  mutate(nro = popER*prop_sini_exp_rural*(1+0.02)^{year-2023}*factor,
         mes_nro = nro*prop_diasidem_sini_rural/30) %>%
  left_join(montoM1,by ='year') %>%
  mutate(costo = valor_incap*mes_nro/scl) %>%
  group_by(year) %>% summarise(inc = sum(nro),mes_inc = sum(mes_nro),
                               costo_inc = sum(costo))
# Permanent disability
indem_per_Pes <- ER1 %>% group_by(year,Sexo) %>%
  summarise(popER = sum(popER),.groups = 'drop') %>%
  left_join(permanente,by = 'Sexo') %>%
  left_join(factores,by = 'tipo') %>%
  mutate(nro = popER*prop_sini_exp_rural*(1+0.02)^{year-2023}*factor) %>%
  left_join(montoM1,by ='year') %>%
  bind_cols(indemnizacion) %>%
  mutate(costo_per = valor_smmlv*Permanente*nro/scl) %>%
  group_by(year) %>% summarise(per = sum(nro),costo_per = sum(costo_per))
M1_Pes <- indem_inc_Pes %>% left_join(indem_per_Pes,by = 'year') %>%
  left_join(muerte_auxseg,by = 'year') %>% rowwise %>%
  mutate(total = sum(costo_inc,costo_per,costo_aux,costo_seg),
         personas = sum(inc,per,muertes)) %>%
  ungroup %>% left_join(ER1 %>% group_by(year) %>%
                          summarise(popER = sum(popER),.groups = 'drop'),
                        by = 'year') %>%
  mutate(PP = total*scl/popER,ER_M1Pes = popER,PC_M1Pes = PP/(1-0.21),
         totalM1Pes = PC_M1Pes*popER/scl)

M1_Base
M1_Opt
M1_Pes

################################################################################
# -------------------- | Model 2. Subsidio rural | ------------------------ ####
# Monetary values
montoM2 <- crecSMMLV %>% mutate(valor_cot = smmlv) %>% select(-smmlv)
montoM2

# Rural subsidies
M2 <- ER2 %>% left_join(cotARL,by = 'CR') %>% left_join(montoM2,by = 'year') %>%
  mutate(costo_cot = popER*valor_cot*prop_cot*12/scl,
         PC_M2 = costo_cot*scl/popER)

M2

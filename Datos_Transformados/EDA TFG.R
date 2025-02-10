# Librerias necesarias
library(readxl)
library(tidyverse)
library(dplyr)
library(skimr)
library(corrplot)


# Carga de los df limpios 
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Ingenieria del dato/Conjunto de datos LIMPIO")
df_au <- read.csv("df_au_limpio.csv")
df_ed <- read.csv("df_ed_limpio.csv")
df_inp <- read.csv("df_inp_limpio.csv")

df_au <- df_au %>% select(-"X")
df_ed <- df_ed %>% select(-"X")
df_inp <- df_inp %>% select(-"X")

# Summary
skim(df_au %>% select(where(is.numeric)) %>% select(-FAC_NO))
skim(df_au %>% select(contains("TOT")))
skim(df_ed %>% select(-"oshpd_id2"))
skim(df_inp)

# Análisis de la tendencia --------------------------------------------------------
df_tendencia_ED <- df_au %>%
  group_by(YEAR) %>%  
  summarize(ER_TRAFFIC_TOT = sum(ER_TRAFFIC_TOT, na.rm = TRUE)) 

df_tendencia_INP <- df_au %>%
  group_by(YEAR) %>%  
  summarize(ADMITTED_FROM_EMER_DEPT_TOT = sum(ADMITTED_FROM_EMER_DEPT_TOT, na.rm = TRUE)) 

write.csv(df_tendencia_ED,"df_tendencia_ED.csv")
write.csv(df_tendencia_INP,"df_tendencia_INP.csv")

library(tseries)
ts_ED <- ts(df_tendencia_ED$ER_TRAFFIC_TOT,start = 2012, frequency = 1)
ts_INP <- ts(df_tendencia_INP$ADMITTED_FROM_EMER_DEPT_TOT,start = 2012, frequency = 1)

# FAC y FACP
par(mfrow=c(1,2))
acf(ts_ED)
pacf(ts_ED)

par(mfrow=c(1,2))
acf(ts_INP)
pacf(ts_INP)


# Test de Dickey Fuller
adf.test(ts_ED)
adf.test(ts_INP)

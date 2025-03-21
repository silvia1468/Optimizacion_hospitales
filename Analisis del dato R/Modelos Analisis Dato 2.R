library(readxl)
library(dplyr)
library(gplots)
library(ggplot2)
library(plm)
library(car) #VIR
library(lmtest)
library(tidyr)
library(Metrics)
library(tibble)

# Carga de los df limpios ------------------------------------------------------
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Analisis del dato")
df_au <- read.csv("df_au_limpio_090325.csv")
df_ed <- read.csv("df_ed_limpio_090325.csv")
df_inp <- read.csv("df_inp_limpio_090325.csv")

df_au <- df_au %>% select(-"X") %>% rename(FACILITY_NAME = FAC_NAME)
df_ed <- df_ed %>% select(-"X") %>% rename(FAC_NO = oshpd_id2)
df_inp <- df_inp %>% select(-"X")


# Subset hospitales comunes ----------------------------------------------------
df_au$FAC_NO_YEAR <- paste(df_au$FAC_NO, df_au$YEAR, sep = ".")
df_au <- df_au %>% relocate(FAC_NO_YEAR, .before = everything())

df_ed$FAC_NO_YEAR <- paste(df_ed$FAC_NO, df_ed$YEAR, sep = ".")
df_ed <- df_ed %>% relocate(FAC_NO_YEAR, .before = everything())

df_inp$FAC_NO_YEAR <- paste(df_inp$FAC_NO, df_inp$YEAR, sep = ".")
df_inp <- df_inp %>% relocate(FAC_NO_YEAR, .before = everything())

# Los ID comunes para todos los datasets
nombres_au <- df_au$FAC_NO_YEAR
nombres_ed <- df_ed$FAC_NO_YEAR 
nombres_inp <- df_inp$FAC_NO_YEAR

hospitales_comunes <- intersect(intersect(nombres_au, nombres_ed), nombres_inp)

#Filtrado de hospitales para quedarme solo con los comunes
df_au_comun <- df_au[df_au$FAC_NO_YEAR %in% hospitales_comunes, ]
df_ed_comun <- df_ed[df_ed$FAC_NO_YEAR %in% hospitales_comunes, ]


# Unión de los datasets
df_ed_comun <- df_ed_comun %>% select(-c("FACILITY_NAME", "COUNTY_NAME", "LICENSE_CATEGORY_DESC", "DBA_ADDRESS1", 
                                         "DBA_CITY", "DBA_ZIP_CODE", "LICENSED_BED_SIZE", "TRAUMA_CENTER_DESC", 
                                         "MSSA_DESIGNATION", "RURAL_HOSPITAL_DESC", "TEACHING_HOSPITAL_DESC", 
                                         "CONTROL_TYPE_DESC", "ER_SERVICE_LEVEL_DESC", "FAC_NO_YEAR")) 
df_ed_comun <- df_ed_comun %>% relocate(YEAR, .before = everything())

df_unido <- merge(df_au_comun, df_ed_comun, by = c("FAC_NO", "YEAR"), all.x = TRUE)

# Comprobar si hay variables duplicadas
duplicated_vars <- names(df_unido)[grepl("\\.x$|\\.y$", names(df_unido))]
#print(duplicated_vars)

# Comprobar valores nulos 
x <- df_unido %>% summarize(across(everything(), ~sum(is.na(.))))


# Eliminación de variables innecesarias 
df_unido <- df_unido %>% select(-c("a_Age_Unknown", "a_All_Other_Languages", "a_blank_inv_lang", "a_disp_Invalid_Blank", 
                                   "a_disp_Other", "a_dx_Other_Reasons", "a_Eth_Other_Unknown", "a_Other_Care", 
                                   "a_Other_Payer", "a_Other_Unknown", "a_PLS_Other_Unknown", "a_racegrp_inv_blank", 
                                   "a_racegrp_other", "a_Unknown_Eth", "a_Unknown_inv_Payer", "a_Unknown_lang", 
                                   "a_Unknown_Race", "Age_Unknown", "All_Other", "All_Other_Languages", 
                                   "blank_inv_lang", "disp_Invalid_Blank", "disp_Other", "dx_Other_Unknown", 
                                   "EC_Other_Accidents", "EC_Other_Factors", "eth_Blank_Invalid", "eth_Unknown", 
                                   "Other_Care", "Other_Payer", "Other_Unknown", "PLS_Other_Unknown", 
                                   "racegrp_inv_blank", "racegrp_other", "racegrp_unknown", "Sex_Other_Unknown", 
                                   "Unknown_inv_Payer", "Unknown_lang", "EMER_DEPT_HR_DIVERSION_JAN", 
                                   "EMER_DEPT_HR_DIVERSION_FEB", "EMER_DEPT_HR_DIVERSION_MAR", 
                                   "EMER_DEPT_HR_DIVERSION_APR", "EMER_DEPT_HR_DIVERSION_MAY", 
                                   "EMER_DEPT_HR_DIVERSION_JUN", "EMER_DEPT_HR_DIVERSION_JUL", 
                                   "EMER_DEPT_HR_DIVERSION_AUG", "EMER_DEPT_HR_DIVERSION_SEP", 
                                   "EMER_DEPT_HR_DIVERSION_OCT", "EMER_DEPT_HR_DIVERSION_NOV", 
                                   "EMER_DEPT_HR_DIVERSION_DEC", "INPATIENT_SURG_OPER_RM_MINS", 
                                   "OUTPATIENT_SURG_OPER_RM_MINS", "Zip_Foreign"))



# Comprobar que no hay filas duplicadas

if (any(table(df_unido$FAC_NO, df_unido$YEAR) != 1)) {
  
  # Identificar los ID de FAC_NO_YEAR no únicos
  conteo_combinaciones <- table(df_unido$FAC_NO_YEAR)  # Contar ocurrencias
  
  combinaciones_no_unicas <- names(conteo_combinaciones[conteo_combinaciones > 1])  # Solo combinaciones duplicadas
  df_unido[df_unido$FAC_NO_YEAR %in% combinaciones_no_unicas, ]
  
  # Filtrar las filas duplicadas según el identificador combinado
  filas_duplicadas <- df_unido[df_unido$FAC_NO_YEAR %in% combinaciones_no_unicas, ]
  
  # Agrupar y resumir duplicados
  datos_duplicados <- filas_duplicadas %>%
    group_by(FAC_NO_YEAR) %>%
    summarise(
      across(where(is.numeric) & !c("YEAR", "FAC_NO"), sum, na.rm = TRUE),  # Sumar las variables numéricas (excepto YEAR y FAC_NO)
      across(where(is.character), first),  # Tomar el primer valor de los caracteres
      across(where(is.factor), first),     # Tomar el primer valor de los factores
      .groups = "drop"
    )
  
  # Eliminar las filas duplicadas originales del df_unido
  df_unido_sin_duplicados <- df_unido[!df_unido$FAC_NO_YEAR %in% combinaciones_no_unicas, ]
  
  # Unir el dataframe sin duplicados con los datos combinados
  df_final <- bind_rows(df_unido_sin_duplicados, datos_duplicados)
  
  # Ordenar por FAC_NO_YEAR
  df_final <- df_final %>% arrange(FAC_NO_YEAR)
  
  df_final[df_final$FAC_NO_YEAR == "106190587.2013", "YEAR"] <- 2013
  df_final[df_final$FAC_NO_YEAR == "106190587.2013", "FAC_NO"] <- 106190587
  
  print("Se detectaron y corrigieron los hospitales duplicados")
  
} else {
  df_final <- df_unido
  print(" No se detectaron hospitales duplicados")
}


# df_unido <- df_unido %>% mutate_if(is.character, as.factor)
# df_unido <- df_unido %>% mutate_if(is.factor, as.numeric)
# 
# setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Analisis del dato") 
# library(openxlsx)
# write.csv(df_unido,"df_unido_num.csv")


# Gráfico heterogeneidad ----------------------------------------------------------------------------------
plotmeans(ER_TRAFFIC_TOT ~ FAC_NO, data= df_final, main= "Heterogeneidad entre hospitales en urgencias", 
          xlab = "ID Hospital", ylab = "Nº Pacientes")
plotmeans(ER_TRAFFIC_TOT ~ YEAR, data= df_final, main= "Heterogeneidad entre hospitales en urgencias", xlab = "ID Hospital", ylab = "Nº Pacientes")


# Dividir conjunto en train y test ----------------------------------------------------------------------
set.seed(42)

# Test: hasta 2020
df_train <- df_final %>% filter(YEAR <= 2021)

# Train: 2021 a 2023
df_test <- df_final %>% filter(YEAR > 2021)

# Convertir a data frame de panel
df_train <- pdata.frame(df_train, index = c("FAC_NO", "YEAR"))
df_test <- pdata.frame(df_test, index = c("FAC_NO", "YEAR"))
class(df_train)
class(df_test)


# Modelo pooled OLS --------------------------------------------------------------------------------------
modelo_pool <- plm(ER_TRAFFIC_TOT ~ TOT_LIC_BEDS + TOT_LIC_BED_DAYS + TOT_DISCHARGES + TOT_CEN_DAYS + AVAIL_SERVICES_ANESTHESIOLOGIST_ON_CALL + 
                     AVAIL_SERVICES_OPER_RM_ON_CALL + AVAIL_SERVICES_PHARMACIST_ON_CALL + EMSA_TRAUMA_DESIGNATION +
                     AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL + AVAIL_SERVICES_RADIOLOGY_ON_CALL + 
                     ADMITTED_FROM_EMER_DEPT_TOT + NON_EMER_VISITS_IN_EMER_DEPT + EMSA_TRAUMA_DESIGNATION_PEDIATRIC + 
                     EMER_REGISTRATIONS_PATS_LEAVE_WO_BEING_SEEN + EMER_DEPT_HR_DIVERSION_TOT + INPATIENT_SURG_OPER + OUTPATIENT_SURG_OPER + 
                     CARDIAC_CATHETERIZATION_DIAGNOSTIC_VISITS_TOT + CARDIAC_CATHETERIZATION_THERAPEUTIC_VISITS_TOT + TOT_ALOS_PY + TOT_ALOS_CY + 
                     Sex_Female + Age_0_09 + Age_10_19 + Age_20_29 + Age_30_39 + Age_40_49 + Age_50_59 + Age_60_69 + Age_70_79 + Age_80_ + 
                     Acute_Care + Against_Medical_Advice + Died + Home_Health_Service + Hospice_Care + Prison_Jail + Psychiatric_Care + 
                     Residential_Care + Routine + SN_IC_Care + Childrens_or_Cancer + CAH + dx_Pregnancy_Childbirth + dx_Diseases_of_the_Blood + 
                     dx_Circulatory + dx_Congenital + dx_Digestive + dx_Endocrine + dx_Genitourinary + dx_Infectious + dx_Injury_Poisoning + 
                     dx_Musculoskeletal + dx_Neoplasms + dx_Nervous_System + dx_Certain_Perinatal_Conditions + dx_Psychoses_Neuroses + 
                     dx_Respiratory + dx_Skin + dx_Symptoms_Signs_NEC + dx_Births + dx_Unacceptable_principal_diagno + EC_Accidental_Falls + 
                     EC_Accidental_Poisoning + EC_Adverse_Effects_of_Drugs + EC_Fire_Accidents + EC_Inflicted_by_Others + EC_Late_Effects_of_Injury + 
                     EC_Misadventures_Complication + EC_Natural_Environment + EC_Self_Inflicted + EC_Submersion_Suffocation_Foreig + EC_War + 
                     English + Spanish + EC_All_Transport_Types + Rehab + Disaster_Care_Site + dx_Birth_Defects + eth_Hispanic + eth_NonHispanic + 
                     dx_Residual + dx_Factors_Influencing_Health_St + dx_Cancer + dx_Ear + dx_Eye + EC_Abnormal_Reaction_Later_Cx + 
                     EC_Med_Device_Advrs_Incident + EC_NonTrans_Drowning_Subm + EC_Supplementary_Factors + EC_W_Inanimate_Animate_Object + 
                     Non.Hispanic.or.Non.Latino + racegrp_aman + racegrp_asian + racegrp_black + racegrp_multirace + racegrp_nhpi + racegrp_white,
                   data = df_train, model = "pooling")


# Test de Breusch - Pagan para ver si hay efectos individuales 
plmtest(modelo_pool, effect = "individual", type = "bp")   
plmtest(modelo_pool, effect = "time", type = "bp")
plmtest(modelo_pool, effect = "twoways", type = "bp")



# Entrenamiento de modelos efectos fijos y efectos aleatorios ------------------------------------------------------------------------------

modelo_FE_A <- plm(ER_TRAFFIC_TOT ~ Sex_Female + Age_0_09 + Age_10_19 + Age_20_29 + Age_30_39 + Age_40_49 + Age_50_59 + Age_60_69 + Age_70_79 + Age_80_ + 
                     Acute_Care + Against_Medical_Advice + Died + Home_Health_Service + Hospice_Care + Prison_Jail + Psychiatric_Care + 
                     Residential_Care + Routine + SN_IC_Care + Childrens_or_Cancer + CAH + dx_Pregnancy_Childbirth + dx_Diseases_of_the_Blood + 
                     dx_Circulatory + dx_Congenital + dx_Digestive + dx_Endocrine + dx_Genitourinary + dx_Infectious + dx_Injury_Poisoning + 
                     dx_Musculoskeletal + dx_Neoplasms + dx_Nervous_System + dx_Certain_Perinatal_Conditions + dx_Psychoses_Neuroses + 
                     dx_Respiratory + dx_Skin + dx_Symptoms_Signs_NEC + dx_Births + dx_Unacceptable_principal_diagno + EC_Accidental_Falls + 
                     EC_Accidental_Poisoning + EC_Adverse_Effects_of_Drugs + EC_Fire_Accidents + EC_Inflicted_by_Others + EC_Late_Effects_of_Injury + 
                     EC_Misadventures_Complication + EC_Natural_Environment + EC_Self_Inflicted + EC_Submersion_Suffocation_Foreig + EC_War + 
                     English + Spanish + EC_All_Transport_Types + Rehab + Disaster_Care_Site + dx_Birth_Defects + eth_Hispanic + eth_NonHispanic + 
                     dx_Residual + dx_Factors_Influencing_Health_St + dx_Cancer + dx_Ear + dx_Eye + EC_Abnormal_Reaction_Later_Cx + 
                     EC_Med_Device_Advrs_Incident + EC_NonTrans_Drowning_Subm + EC_Supplementary_Factors + EC_W_Inanimate_Animate_Object + 
                     Non.Hispanic.or.Non.Latino + racegrp_aman + racegrp_asian + racegrp_black + racegrp_multirace + racegrp_nhpi + racegrp_white,
                   data = df_train, model = "within")

modelo_FE_B <- plm(ER_TRAFFIC_TOT ~ TOT_LIC_BEDS + TOT_LIC_BED_DAYS + TOT_DISCHARGES + TOT_CEN_DAYS + AVAIL_SERVICES_ANESTHESIOLOGIST_ON_CALL + 
                     AVAIL_SERVICES_OPER_RM_ON_CALL + AVAIL_SERVICES_PHARMACIST_ON_CALL + EMSA_TRAUMA_DESIGNATION +
                     AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL + AVAIL_SERVICES_RADIOLOGY_ON_CALL + 
                     ADMITTED_FROM_EMER_DEPT_TOT + NON_EMER_VISITS_IN_EMER_DEPT + EMSA_TRAUMA_DESIGNATION_PEDIATRIC + 
                     EMER_REGISTRATIONS_PATS_LEAVE_WO_BEING_SEEN + EMER_DEPT_HR_DIVERSION_TOT + INPATIENT_SURG_OPER + OUTPATIENT_SURG_OPER + 
                     CARDIAC_CATHETERIZATION_DIAGNOSTIC_VISITS_TOT + CARDIAC_CATHETERIZATION_THERAPEUTIC_VISITS_TOT + TOT_ALOS_PY + TOT_ALOS_CY + 
                     Sex_Female + Age_0_09 + Age_10_19 + Age_20_29 + Age_30_39 + Age_40_49 + Age_50_59 + Age_60_69 + Age_70_79 + Age_80_ + 
                     Acute_Care + Against_Medical_Advice + Died + Home_Health_Service + Hospice_Care + Prison_Jail + Psychiatric_Care + 
                     Residential_Care + Routine + SN_IC_Care + Childrens_or_Cancer + CAH + dx_Pregnancy_Childbirth + dx_Diseases_of_the_Blood + 
                     dx_Circulatory + dx_Congenital + dx_Digestive + dx_Endocrine + dx_Genitourinary + dx_Infectious + dx_Injury_Poisoning + 
                     dx_Musculoskeletal + dx_Neoplasms + dx_Nervous_System + dx_Certain_Perinatal_Conditions + dx_Psychoses_Neuroses + 
                     dx_Respiratory + dx_Skin + dx_Symptoms_Signs_NEC + dx_Births + dx_Unacceptable_principal_diagno + EC_Accidental_Falls + 
                     EC_Accidental_Poisoning + EC_Adverse_Effects_of_Drugs + EC_Fire_Accidents + EC_Inflicted_by_Others + EC_Late_Effects_of_Injury + 
                     EC_Misadventures_Complication + EC_Natural_Environment + EC_Self_Inflicted + EC_Submersion_Suffocation_Foreig + EC_War + 
                     English + Spanish + EC_All_Transport_Types + Rehab + Disaster_Care_Site + dx_Birth_Defects + eth_Hispanic + eth_NonHispanic + 
                     dx_Residual + dx_Factors_Influencing_Health_St + dx_Cancer + dx_Ear + dx_Eye + EC_Abnormal_Reaction_Later_Cx + 
                     EC_Med_Device_Advrs_Incident + EC_NonTrans_Drowning_Subm + EC_Supplementary_Factors + EC_W_Inanimate_Animate_Object + 
                     Non.Hispanic.or.Non.Latino + racegrp_aman + racegrp_asian + racegrp_black + racegrp_multirace + racegrp_nhpi + racegrp_white, 
                   data = df_train, model = "within")  


# Modelos de efectos aleatorios 
modelo_RE_A <- plm(formula(modelo_FE_A), data = df_train, model = "random")
modelo_RE_B <- plm(formula(modelo_FE_B), data = df_train, model = "random")

# Test de significación
pFtest(modelo_FE_A, modelo_pool)
pFtest(modelo_FE_B, modelo_pool)
 # evalua si hay efectos individuales significativos. Ambos salen menor que pvalor => hay efectos individuales significativos. luego conviene utilizar un modelo de panel

# Test de Hausman: indica que modelo es mejor entre efectos fijos (p<0.05) y aleatorios (>0.05)
phtest(modelo_FE_A, modelo_RE_A)
phtest(modelo_FE_B, modelo_RE_B)

  # el modelo es muy sifnificativo porqlo que el mejor modelo es el de EF

summary(modelo_FE_A)
summary(modelo_FE_B)


# Eliminan las variables no significativas --------------------------------------------------------------------------------------
modelo_FE_A <- plm(ER_TRAFFIC_TOT ~ Sex_Female + Age_0_09 + Age_10_19 + Age_20_29 + Age_30_39 + Age_40_49 + Age_50_59 + Age_60_69 + Age_70_79  + 
                      SN_IC_Care + Childrens_or_Cancer  + dx_Pregnancy_Childbirth +  Psychiatric_Care + Age_80_ +
                     Residential_Care + Routine + Against_Medical_Advice + Died + 
                     dx_Circulatory + dx_Congenital + dx_Digestive + dx_Endocrine + dx_Genitourinary + dx_Infectious + dx_Injury_Poisoning + 
                     dx_Musculoskeletal + dx_Neoplasms + dx_Nervous_System + dx_Certain_Perinatal_Conditions + dx_Psychoses_Neuroses + 
                     dx_Respiratory + dx_Symptoms_Signs_NEC +  dx_Unacceptable_principal_diagno + EC_Accidental_Falls + 
                     EC_Accidental_Poisoning + EC_Adverse_Effects_of_Drugs + EC_Fire_Accidents + EC_Inflicted_by_Others + EC_Late_Effects_of_Injury + 
                     EC_Misadventures_Complication + EC_Natural_Environment + EC_Self_Inflicted + EC_Submersion_Suffocation_Foreig + EC_War + 
                     English + Spanish + EC_All_Transport_Types + Rehab + Disaster_Care_Site + dx_Birth_Defects + eth_Hispanic + eth_NonHispanic + 
                     dx_Residual + dx_Factors_Influencing_Health_St + dx_Cancer + dx_Ear + dx_Eye + EC_Abnormal_Reaction_Later_Cx + 
                     EC_Med_Device_Advrs_Incident + EC_NonTrans_Drowning_Subm + EC_Supplementary_Factors + EC_W_Inanimate_Animate_Object + 
                     Non.Hispanic.or.Non.Latino + racegrp_aman + racegrp_asian + racegrp_black + racegrp_white,
                   data = df_train, model = "within")


modelo_FE_B <- plm(ER_TRAFFIC_TOT ~ TOT_LIC_BEDS + TOT_LIC_BED_DAYS + TOT_DISCHARGES + TOT_CEN_DAYS + AVAIL_SERVICES_ANESTHESIOLOGIST_ON_CALL + 
                     AVAIL_SERVICES_OPER_RM_ON_CALL + AVAIL_SERVICES_PHARMACIST_ON_CALL +
                     AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL + AVAIL_SERVICES_RADIOLOGY_ON_CALL + 
                     ADMITTED_FROM_EMER_DEPT_TOT + NON_EMER_VISITS_IN_EMER_DEPT + EMSA_TRAUMA_DESIGNATION_PEDIATRIC + 
                     EMER_REGISTRATIONS_PATS_LEAVE_WO_BEING_SEEN + EMER_DEPT_HR_DIVERSION_TOT + INPATIENT_SURG_OPER + OUTPATIENT_SURG_OPER + 
                     CARDIAC_CATHETERIZATION_DIAGNOSTIC_VISITS_TOT + CARDIAC_CATHETERIZATION_THERAPEUTIC_VISITS_TOT +  
                     Sex_Female + Age_0_09 + Age_10_19 + Age_20_29 + Age_30_39 + Age_40_49 + Age_50_59 + Age_60_69 + Age_70_79 + Age_80_ + 
                     Acute_Care + Against_Medical_Advice + Home_Health_Service + Hospice_Care + Prison_Jail + Psychiatric_Care + 
                     Residential_Care + Routine + SN_IC_Care + Childrens_or_Cancer + CAH + dx_Pregnancy_Childbirth + 
                     dx_Circulatory + dx_Congenital + dx_Digestive + dx_Endocrine + dx_Infectious + dx_Injury_Poisoning + 
                     dx_Musculoskeletal + dx_Neoplasms + dx_Nervous_System + dx_Certain_Perinatal_Conditions + dx_Psychoses_Neuroses + 
                     dx_Respiratory + dx_Skin + dx_Symptoms_Signs_NEC + dx_Births + dx_Unacceptable_principal_diagno + EC_Accidental_Falls + 
                     EC_Accidental_Poisoning + EC_Adverse_Effects_of_Drugs + EC_Fire_Accidents + EC_Inflicted_by_Others + EC_Late_Effects_of_Injury + 
                     EC_Misadventures_Complication + EC_Natural_Environment + EC_Self_Inflicted + EC_Submersion_Suffocation_Foreig + EC_War + 
                     English + Spanish + EC_All_Transport_Types + Rehab + Disaster_Care_Site + dx_Birth_Defects + eth_Hispanic + eth_NonHispanic + 
                     dx_Residual + dx_Factors_Influencing_Health_St + dx_Cancer + dx_Ear + dx_Eye + EC_Abnormal_Reaction_Later_Cx + 
                     EC_Med_Device_Advrs_Incident + EC_NonTrans_Drowning_Subm + EC_Supplementary_Factors + EC_W_Inanimate_Animate_Object + 
                     Non.Hispanic.or.Non.Latino + racegrp_aman + racegrp_asian + racegrp_black + racegrp_multirace + racegrp_nhpi + racegrp_white, 
                   data = df_train, model = "within")  

modelo_FE_B_reducido <- plm(ER_TRAFFIC_TOT ~ TOT_LIC_BEDS + TOT_LIC_BED_DAYS + TOT_DISCHARGES + TOT_CEN_DAYS +
                              AVAIL_SERVICES_OPER_RM_ON_CALL + AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL + ADMITTED_FROM_EMER_DEPT_TOT + 
                              NON_EMER_VISITS_IN_EMER_DEPT + EMSA_TRAUMA_DESIGNATION_PEDIATRIC + EMER_REGISTRATIONS_PATS_LEAVE_WO_BEING_SEEN + 
                              EMER_DEPT_HR_DIVERSION_TOT + OUTPATIENT_SURG_OPER + CARDIAC_CATHETERIZATION_THERAPEUTIC_VISITS_TOT + Sex_Female + 
                              Age_50_59 + Age_60_69 + Age_70_79  + Hospice_Care + Residential_Care + SN_IC_Care + Childrens_or_Cancer + 
                              dx_Pregnancy_Childbirth + dx_Circulatory + dx_Congenital + dx_Musculoskeletal +
                              dx_Symptoms_Signs_NEC + EC_Inflicted_by_Others + EC_Late_Effects_of_Injury + Disaster_Care_Site +
                              EC_NonTrans_Drowning_Subm + Non.Hispanic.or.Non.Latino + racegrp_asian,
                            data = df_train,  model = "within")
summary(modelo_FE_B_reducido)



# Predicción del modelo para A -------------------------------------------------------------------------------
vars_modelo_A <- c("Sex_Female", "Age_0_09", "Age_10_19", "Age_20_29", "Age_30_39", "Age_40_49", "Age_50_59", "Age_60_69", "Age_70_79", "Age_80_",
                   "Against_Medical_Advice", "Died", "Psychiatric_Care",
                   "Residential_Care", "Routine", "SN_IC_Care", "Childrens_or_Cancer", "dx_Pregnancy_Childbirth",
                   "dx_Circulatory", "dx_Congenital", "dx_Digestive", "dx_Endocrine", "dx_Genitourinary", "dx_Infectious", "dx_Injury_Poisoning",
                   "dx_Musculoskeletal", "dx_Neoplasms", "dx_Nervous_System", "dx_Certain_Perinatal_Conditions", "dx_Psychoses_Neuroses",
                   "dx_Respiratory", "dx_Symptoms_Signs_NEC", "dx_Unacceptable_principal_diagno", "EC_Accidental_Falls",
                   "EC_Accidental_Poisoning", "EC_Adverse_Effects_of_Drugs", "EC_Fire_Accidents", "EC_Inflicted_by_Others", "EC_Late_Effects_of_Injury",
                   "EC_Misadventures_Complication", "EC_Natural_Environment", "EC_Self_Inflicted", "EC_Submersion_Suffocation_Foreig", "EC_War",
                   "English", "Spanish", "EC_All_Transport_Types", "Rehab", "Disaster_Care_Site", "dx_Birth_Defects", "eth_Hispanic", "eth_NonHispanic",
                   "dx_Residual", "dx_Factors_Influencing_Health_St", "dx_Cancer", "dx_Ear", "dx_Eye", "EC_Abnormal_Reaction_Later_Cx",
                   "EC_Med_Device_Advrs_Incident", "EC_NonTrans_Drowning_Subm", "EC_Supplementary_Factors", "EC_W_Inanimate_Animate_Object",
                   "Non.Hispanic.or.Non.Latino", "racegrp_aman", "racegrp_asian", "racegrp_black", "racegrp_white")


# Extraer variables del conjunto de test
X_test_A <- as.matrix(df_test[, vars_modelo_A])

# Obtener coeficientes del modelo FE A
coef_FE_A <- coef(modelo_FE_A)

dim(X_test_A)            # Debe ser filas x columnas (n x k)
length(coef_FE_A)        # Debe ser igual al número de columnas (k)

# ¿Hay diferencias entre nombres de columnas y nombres de coeficientes?
setdiff(colnames(X_test_A), names(coef_FE_A))  # Columnas de más en X_test_A
setdiff(names(coef_FE_A), colnames(X_test_A))  # Columnas faltantes en X_test_A

# Predección
pred_FE_A <- X_test_A %*% coef_FE_A
efectos_FE_A <- fixef(modelo_FE_A)
pred_FE_A_con_efectos <- pred_FE_A + efectos_FE_A[as.character(df_test$FAC_NO)]

head(pred_FE_A_con_efectos)



# Predicción del modelo para B -------------------------------------------------------------------------------

vars_modelo_B <- c("TOT_LIC_BEDS", "TOT_LIC_BED_DAYS", "TOT_DISCHARGES", "TOT_CEN_DAYS", "AVAIL_SERVICES_ANESTHESIOLOGIST_ON_CALL",
                   "AVAIL_SERVICES_OPER_RM_ON_CALL", "AVAIL_SERVICES_PHARMACIST_ON_CALL", "EMSA_TRAUMA_DESIGNATION",
                   "AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL", "AVAIL_SERVICES_RADIOLOGY_ON_CALL",
                   "ADMITTED_FROM_EMER_DEPT_TOT", "NON_EMER_VISITS_IN_EMER_DEPT", "EMSA_TRAUMA_DESIGNATION_PEDIATRIC",
                   "EMER_REGISTRATIONS_PATS_LEAVE_WO_BEING_SEEN", "EMER_DEPT_HR_DIVERSION_TOT", "INPATIENT_SURG_OPER", "OUTPATIENT_SURG_OPER",
                   "CARDIAC_CATHETERIZATION_DIAGNOSTIC_VISITS_TOT", "CARDIAC_CATHETERIZATION_THERAPEUTIC_VISITS_TOT", "TOT_ALOS_PY", "TOT_ALOS_CY",
                   "Sex_Female", "Age_0_09", "Age_10_19", "Age_20_29", "Age_30_39", "Age_40_49", "Age_50_59", "Age_60_69", "Age_70_79", "Age_80_",
                   "Acute_Care", "Against_Medical_Advice", "Died", "Home_Health_Service", "Hospice_Care", "Prison_Jail", "Psychiatric_Care",
                   "Residential_Care", "Routine", "SN_IC_Care", "Childrens_or_Cancer", "CAH", "dx_Pregnancy_Childbirth", "dx_Diseases_of_the_Blood",
                   "dx_Circulatory", "dx_Congenital", "dx_Digestive", "dx_Endocrine", "dx_Genitourinary", "dx_Infectious", "dx_Injury_Poisoning",
                   "dx_Musculoskeletal", "dx_Neoplasms", "dx_Nervous_System", "dx_Certain_Perinatal_Conditions", "dx_Psychoses_Neuroses",
                   "dx_Respiratory", "dx_Skin", "dx_Symptoms_Signs_NEC", "dx_Births", "dx_Unacceptable_principal_diagno", "EC_Accidental_Falls",
                   "EC_Accidental_Poisoning", "EC_Adverse_Effects_of_Drugs", "EC_Fire_Accidents", "EC_Inflicted_by_Others", "EC_Late_Effects_of_Injury",
                   "EC_Misadventures_Complication", "EC_Natural_Environment", "EC_Self_Inflicted", "EC_Submersion_Suffocation_Foreig", "EC_War",
                   "English", "Spanish", "EC_All_Transport_Types", "Rehab", "Disaster_Care_Site", "dx_Birth_Defects", "eth_Hispanic", "eth_NonHispanic",
                   "dx_Residual", "dx_Factors_Influencing_Health_St", "dx_Cancer", "dx_Ear", "dx_Eye", "EC_Abnormal_Reaction_Later_Cx",
                   "EC_Med_Device_Advrs_Incident", "EC_NonTrans_Drowning_Subm", "EC_Supplementary_Factors", "EC_W_Inanimate_Animate_Object",
                   "Non.Hispanic.or.Non.Latino", "racegrp_aman", "racegrp_asian", "racegrp_black", "racegrp_multirace", "racegrp_nhpi", "racegrp_white")


vars_modelo_B <- c("TOT_LIC_BEDS", "TOT_LIC_BED_DAYS", "TOT_DISCHARGES", "TOT_CEN_DAYS", "AVAIL_SERVICES_ANESTHESIOLOGIST_ON_CALL",
                   "AVAIL_SERVICES_OPER_RM_ON_CALL", "AVAIL_SERVICES_PHARMACIST_ON_CALL",
                   "AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL", "AVAIL_SERVICES_RADIOLOGY_ON_CALL",
                   "ADMITTED_FROM_EMER_DEPT_TOT", "NON_EMER_VISITS_IN_EMER_DEPT", "EMSA_TRAUMA_DESIGNATION_PEDIATRIC",
                   "EMER_REGISTRATIONS_PATS_LEAVE_WO_BEING_SEEN", "EMER_DEPT_HR_DIVERSION_TOT", "INPATIENT_SURG_OPER", "OUTPATIENT_SURG_OPER",
                   "CARDIAC_CATHETERIZATION_DIAGNOSTIC_VISITS_TOT", "CARDIAC_CATHETERIZATION_THERAPEUTIC_VISITS_TOT",
                   "Sex_Female", "Age_0_09", "Age_10_19", "Age_20_29", "Age_30_39", "Age_40_49", "Age_50_59", "Age_60_69", "Age_70_79", "Age_80_",
                   "Acute_Care", "Against_Medical_Advice", "Home_Health_Service", "Hospice_Care", "Prison_Jail", "Psychiatric_Care",
                   "Residential_Care", "Routine", "SN_IC_Care", "Childrens_or_Cancer", "CAH", "dx_Pregnancy_Childbirth",
                   "dx_Circulatory", "dx_Congenital", "dx_Digestive", "dx_Endocrine", "dx_Infectious", "dx_Injury_Poisoning",
                   "dx_Musculoskeletal", "dx_Neoplasms", "dx_Nervous_System", "dx_Certain_Perinatal_Conditions", "dx_Psychoses_Neuroses",
                   "dx_Respiratory", "dx_Skin", "dx_Symptoms_Signs_NEC", "dx_Births", "dx_Unacceptable_principal_diagno", "EC_Accidental_Falls",
                   "EC_Accidental_Poisoning", "EC_Adverse_Effects_of_Drugs", "EC_Fire_Accidents", "EC_Inflicted_by_Others", "EC_Late_Effects_of_Injury",
                   "EC_Misadventures_Complication", "EC_Natural_Environment", "EC_Self_Inflicted", "EC_Submersion_Suffocation_Foreig", "EC_War",
                   "English", "Spanish", "EC_All_Transport_Types", "Rehab", "Disaster_Care_Site", "dx_Birth_Defects", "eth_Hispanic", "eth_NonHispanic",
                   "dx_Residual", "dx_Factors_Influencing_Health_St", "dx_Cancer", "dx_Ear", "dx_Eye", "EC_Abnormal_Reaction_Later_Cx",
                   "EC_Med_Device_Advrs_Incident", "EC_NonTrans_Drowning_Subm", "EC_Supplementary_Factors", "EC_W_Inanimate_Animate_Object",
                   "Non.Hispanic.or.Non.Latino", "racegrp_aman", "racegrp_asian", "racegrp_black", "racegrp_multirace", "racegrp_nhpi", "racegrp_white")


# Extraer variables del conjunto de test

X_test_B <- as.matrix(df_test[, vars_modelo_B])
X_test_B <- as.data.frame(X_test_B)

# Obtener coeficientes del modelo FE B
coef_FE_B <- coef(modelo_FE_B)

dim(X_test_B)        # Debe ser filas x columnas (n x k)
length(coef_FE_B)    # Debe ser igual al número de columnas (k)

# Identificar diferencias entre test y coeficientes
faltantes <- setdiff(names(coef_FE_B), colnames(X_test_B))  # Columnas que faltan
sobrantes <- setdiff(colnames(X_test_B), names(coef_FE_B))  # Columnas que sobran

# Agregar columnas faltantes al test con valor 0
for (var in faltantes) {
  X_test_B[[var]] <- 0
}

# Eliminar columnas sobrantes
if (length(sobrantes) > 0) {
  X_test_B <- X_test_B[, !(colnames(X_test_B) %in% sobrantes)]
}

# Reordenar columnas en el orden de los coeficientes
X_test_B <- X_test_B[, names(coef_FE_B), drop = FALSE]

# Convertir character -> numeric
X_test_B[] <- lapply(X_test_B, function(x) {  
  if (is.factor(x)) {
    as.numeric(as.character(x))  
  } else if (is.character(x)) {
    as.numeric(x)  
  } else {
    x
  }
})
str(X_test_B)


# Convertir a matriz
X_test_B_matrix <- as.matrix(X_test_B)

# Predecir
pred_FE_B <- X_test_B_matrix %*% coef_FE_B

efectos_FE_B <- fixef(modelo_FE_B)
pred_FE_B_con_efectos <- pred_FE_B + efectos_FE_B[as.character(df_test$FAC_NO)]

head(pred_FE_B_con_efectos)



# Predicción del modelo para B REDUCIDO -------------------------------------------------------------------------------

vars_modelo_B_reducido <- c(
  "TOT_LIC_BEDS", "TOT_LIC_BED_DAYS", "TOT_DISCHARGES", "TOT_CEN_DAYS",
  "AVAIL_SERVICES_OPER_RM_ON_CALL", "AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL",
  "ADMITTED_FROM_EMER_DEPT_TOT", "NON_EMER_VISITS_IN_EMER_DEPT",
  "EMSA_TRAUMA_DESIGNATION_PEDIATRIC", "EMER_REGISTRATIONS_PATS_LEAVE_WO_BEING_SEEN",
  "EMER_DEPT_HR_DIVERSION_TOT", "INPATIENT_SURG_OPER", "OUTPATIENT_SURG_OPER",
  "CARDIAC_CATHETERIZATION_THERAPEUTIC_VISITS_TOT", "Sex_Female", "Age_50_59",
  "Age_60_69", "Age_70_79", "Psychiatric_Care", "Hospice_Care",
  "Residential_Care", "SN_IC_Care", "Childrens_or_Cancer", "dx_Pregnancy_Childbirth",
  "dx_Circulatory", "dx_Congenital", "dx_Musculoskeletal", "dx_Symptoms_Signs_NEC",
  "EC_Inflicted_by_Others", "EC_Late_Effects_of_Injury", "EC_War",
  "Disaster_Care_Site", "EC_NonTrans_Drowning_Subm", "EC_Supplementary_Factors",
  "Non.Hispanic.or.Non.Latino", "racegrp_asian", "racegrp_white"
)

# Extraer variables del conjunto de test 
X_test_B_RED <- as.matrix(df_test[, vars_modelo_B_reducido])
X_test_B_RED <- as.data.frame(X_test_B_RED)

# Obtener coeficientes del modelo FE 
coef_FE_B_reducido <- coef(modelo_FE_B_reducido)

dim(X_test_B_RED)                # Filas x columnas
length(coef_FE_B_reducido)       # Número de coeficientes

#Identificar diferencias entre test y coeficientes
faltantes <- setdiff(names(coef_FE_B_reducido), colnames(X_test_B_RED))  # Faltantes
sobrantes <- setdiff(colnames(X_test_B_RED), names(coef_FE_B_reducido))  # Sobrantes

# Agregar columnas faltantes al test con valor 0
for (var in faltantes) {
  X_test_B_RED[[var]] <- 0
}

# Eliminar columnas sobrantes
if (length(sobrantes) > 0) {
  X_test_B_RED <- X_test_B_RED[, !(colnames(X_test_B_RED) %in% sobrantes)]
}

# Reordenar columnas en el orden de los coeficientes
X_test_B_RED <- X_test_B_RED[, names(coef_FE_B_reducido), drop = FALSE]

# Convertir a numérico si es necesario
X_test_B_RED[] <- lapply(X_test_B_RED, function(x) {
  if (is.factor(x)) {
    as.numeric(as.character(x))
  } else if (is.character(x)) {
    as.numeric(x)
  } else {
    x
  }
})

str(X_test_B_RED)

X_test_B_RED_matrix <- as.matrix(X_test_B_RED)

# Predecir
pred_FE_B_reducido <- X_test_B_RED_matrix %*% coef_FE_B_reducido

efectos_FE_B_reducido <- fixef(modelo_FE_B_reducido)
pred_FE_B_reducido_con_efectos <- pred_FE_B_reducido + efectos_FE_B_reducido[as.character(df_test$FAC_NO)]

head(pred_FE_B_reducido_con_efectos)



# Tabla comparativa de modelos ------------------------------------------------------

# Combina las predicciones y los valores reales en un dataframe
df_metricas <- data.frame(
  y_real = df_test$ER_TRAFFIC_TOT,                 # Valores reales
  pred_FE_A = pred_FE_A_con_efectos,               # Predicciones A con efectos fijos
  pred_FE_B = pred_FE_B_con_efectos,               # Predicciones B con efectos fijos
  pred_FE_B_reducido = pred_FE_B_reducido_con_efectos  # Predicciones B RED con efectos fijos
)

# Eliminar cualquier fila con NA
df_metricas <- df_metricas %>%
  filter(!is.na(y_real), !is.na(pred_FE_A), !is.na(pred_FE_B), !is.na(pred_FE_B_reducido))

# Asegurarse que no haya valores negativos (especialmente para RMSLE)
df_metricas <- df_metricas %>%
  filter(y_real >= 0, pred_FE_A >= 0, pred_FE_B >= 0, pred_FE_B_reducido >= 0)

# Extraer vectores limpios para las métricas
y_real_clean <- df_metricas$y_real
pred_FE_A_clean <- df_metricas$pred_FE_A
pred_FE_B_clean <- df_metricas$pred_FE_B
pred_FE_B_reducido_clean <- df_metricas$pred_FE_B_reducido

# Función para calcular métricas 
calcular_metricas <- function(real, pred){
  list(
    RMSE  = rmse(real, pred),
    MAE   = mae(real, pred),
    RMSLE = rmsle(real + 1, pred + 1)  # Sumar 1 para evitar log(0)
  )
}

# Calcular métricas para los modelos
metricas_FE_A <- calcular_metricas(y_real_clean, pred_FE_A_clean)
metricas_FE_B <- calcular_metricas(y_real_clean, pred_FE_B_clean)
metricas_FE_B_reducido <- calcular_metricas(y_real_clean, pred_FE_B_reducido_clean)

# Crear el dataframe comparativo 
resultados_metricas <- data.frame(
  Modelo = c("Modelo_FE_A", "Modelo_FE_B", "Modelo_FE_B_reducido"),
  RMSE   = c(metricas_FE_A$RMSE, metricas_FE_B$RMSE, metricas_FE_B_reducido$RMSE),
  MAE    = c(metricas_FE_A$MAE,  metricas_FE_B$MAE,  metricas_FE_B_reducido$MAE),
  RMSLE  = c(metricas_FE_A$RMSLE, metricas_FE_B$RMSLE, metricas_FE_B_reducido$RMSLE)
)

resultados_metricas[, -1] <- round(resultados_metricas[, -1], 2)

print(resultados_metricas)





# Gráfico comparación de modelos -------------------------------------------------------------------------

# Primero crea un dataframe con tus datos de predicción y real
df_pred_vs_real <- data.frame(
  Real = y_real_clean,
  Pred_FE_A = pred_FE_A_clean,
  Pred_FE_B = pred_FE_B_clean,
  Pred_FE_B_reducido = pred_FE_B_reducido_clean
)

df_long <- df_pred_vs_real %>%
  pivot_longer(cols = c("Pred_FE_A", "Pred_FE_B", "Pred_FE_B_reducido"), names_to = "Modelo", values_to = "Prediccion")

ggplot(df_long, aes(x = Real, y = Prediccion, color = Modelo)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Predicción vs Real - Comparativa de Modelos",
    x = "Valores Reales de Pacientes en Urgencias",
    y = "Predicción"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Pred_FE_A" = "#1f77b4", "Pred_FE_B" = "#ff7f0e","Pred_FE_B_reducido" = "#2ca02c"))




# Gráfico análisis de los residuos
# Residuos modelo A
df_pred_vs_real$resid_FE_A <- df_pred_vs_real$Real - df_pred_vs_real$Pred_FE_A
ggplot(df_pred_vs_real, aes(x = Real, y = resid_FE_A)) +
  geom_point(alpha = 0.6, color = "#1f77b4") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos - Modelo FE A", x = "Valor Real ", y = "Residuo (Real - Predicción)") +
  theme_minimal()

# Residuos modelo B
df_pred_vs_real$resid_FE_B <- df_pred_vs_real$Real - df_pred_vs_real$Pred_FE_B

ggplot(df_pred_vs_real, aes(x = Real, y = resid_FE_B)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuos - Modelo FE B", x = "Valor Real", y = "Residuo (Real - Predicción)") +
  theme_minimal()

# Residuos modelo B reducido
df_pred_vs_real$resid_FE_B_reducido <- df_pred_vs_real$Real - df_pred_vs_real$Pred_FE_B_reducido

ggplot(df_pred_vs_real, aes(x = Real, y = resid_FE_B_reducido)) +
  geom_point(alpha = 0.6, color = "#2ca02c") +  # Color asociado al modelo B reducido en el gráfico anterior
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos - Modelo FE B Reducido", x = "Valor Real", y = "Residuo (Real - Predicción)" ) +
  theme_minimal()

# Gráfico de normalidad de los residuos para A
residuos_FE_A <- residuals(modelo_FE_A)

# Histograma de los residuos
par(mfrow = c(1,2))
ggplot(data.frame(residuos = residuos_FE_A), aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma de los residuos - Modelo FE A", x = "Residuos", y = "Densidad")

# QQ Plot
qqnorm(residuos_FE_A, main = "QQ Plot - Residuos Modelo FE A") + qqline(residuos_FE_A, col = "red")


# Gráfico de normalidad de los residuos para B
residuos_FE_B <- residuals(modelo_FE_B)

# Histograma de los residuos
ggplot(data.frame(residuos = residuos_FE_B), aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma de los residuos - Modelo FE A", x = "Residuos", y = "Densidad")

# QQ Plot
qqnorm(residuos_FE_B, main = "QQ Plot - Residuos Modelo FE A")
qqline(residuos_FE_B, col = "red")



# Gráfico de normalidad de los residuos para B Reducido
residuos_FE_B_reducido <- residuals(modelo_FE_B_reducido)

# Histograma de los residuos
ggplot(data.frame(residuos = residuos_FE_B_reducido), aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histograma de los residuos - Modelo FE A", x = "Residuos", y = "Densidad")

# QQ Plot
qqnorm(residuos_FE_B_reducido, main = "QQ Plot - Residuos Modelo FE A")
qqline(residuos_FE_B_reducido, col = "red")




# Gráfico residuos estandarizados
df_residuos_A_long <- residuos_FE_A %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "FAC_NO") %>%
  pivot_longer(
    cols = -FAC_NO,
    names_to = "YEAR",
    values_to = "Residuo"
  ) %>%
  filter(!is.na(Residuo))  

df_residuos_A_long <- df_residuos_A_long %>%
  mutate(Observacion = 1:n())

ggplot(df_residuos_A_long, aes(x = Observacion, y = Residuo)) +
  geom_point(color = "#1f77b4", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red") +
  labs(
    title = "Residuos Estandarizados - Modelo FE A",
    x = "Observación",
    y = "Residuo Estandarizado"
  ) +
  theme_minimal()


# Gráfico residuos estandarizados para el Modelo FE B

df_residuos_B_long <- residuos_FE_B %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "FAC_NO") %>%
  pivot_longer(
    cols = -FAC_NO,
    names_to = "YEAR",
    values_to = "Residuo"
  ) %>%
  filter(!is.na(Residuo))

df_residuos_B_long <- df_residuos_B_long %>%
  mutate(Observacion = 1:n())

ggplot(df_residuos_B_long, aes(x = Observacion, y = Residuo)) +
  geom_point(color = "orange", alpha = 0.6) +  
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red") +
  labs(
    title = "Residuos Estandarizados - Modelo FE B",
    x = "Observación",
    y = "Residuo Estandarizado"
  ) +
  theme_minimal()


# Gráfico residuos estandarizados para el Modelo FE B Reducido

df_residuos_B_reducido_long <- residuos_FE_B_reducido %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "FAC_NO") %>%
  pivot_longer(
    cols = -FAC_NO,
    names_to = "YEAR",
    values_to = "Residuo"
  ) %>%
  filter(!is.na(Residuo))  

df_residuos_B_reducido_long <- df_residuos_B_reducido_long %>%
  mutate(Observacion = 1:n())

ggplot(df_residuos_B_reducido_long, aes(x = Observacion, y = Residuo)) +
  geom_point(color = "#2ca02c", alpha = 0.6) +  
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "red") +
  labs(
    title = "Residuos Estandarizados - Modelo FE B Reducido",
    x = "Observación",
    y = "Residuo Estandarizado"
  ) +
  theme_minimal()




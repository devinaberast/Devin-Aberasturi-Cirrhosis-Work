#FINAL Hospital Days Model

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(ggplot2); library(survminer)

he_data <- fread("Z:/data/devindata/final_dataset")

#Subsetting to Users with hospital days after first HE event

hospital_with_rifax <- subset(he_data, !is.na(he_data$hospdays_he))

hospital_with_rifax$rifax_user_he[is.na(hospital_with_rifax$rifax_user_he)] <- 0

hospital_days_modeldataset <- dplyr::select(hospital_with_rifax, c(1:8,11,16,20:26,32,38,62:67,71))

#Need Natural log of Person year for offset; Link is log for the models

hospital_days_modeldataset$log_personyear_he <- log(hospital_days_modeldataset$personyear_he)

#Subset data with offset that is reasonable

hospital_days_modeldataset <- subset(hospital_days_modeldataset, hospital_days_modeldataset$log_personyear_he != -Inf )

#Change gastroentestinal consult to be marked as no if no record

hospital_days_modeldataset$gc_any[is.na(hospital_days_modeldataset$gc_any)] <- 0

#Format as factor for race group

hospital_days_modeldataset$race_group <- factor(hospital_days_modeldataset$race_group, levels = c("White","Black", "Other"))

library(gamlss)
library(gamlss.tr)

#Creates Zero-truncated Negative Binomial distribution used for models
#Zero-truncation important since we have lots of patients with no record
#Zero-truncation adjusts for no record in modeling

gen.trun(0,"NBI",type= "left", name= "lefttr")

truncated_fullmodel <- gamlss(hospdays_he~ factor(ascites_have) + 
                               factor(varices_have) + factor(tips_have) + factor(hecc_have)+ 
                               age + factor(region) + factor(Urban) + factor(race_group) + 
                               factor(sex) + factor(esrd) +  factor(ccif) + factor(gc_any)+ 
                               factor(HepC)+ factor(HepB) + factor(AlcCirr) +
                               factor(NotAlcCirr) + factor(rifax_user_he)+ offset(log_personyear_he), family= NBIlefttr,
                               data = hospital_days_modeldataset)

summary(truncated_fullmodel)

truncated_unimodel <- gamlss(hospdays_he~ factor(rifax_user_he)+ offset(log_personyear_he), family= NBIlefttr,
                             data = hospital_days_modeldataset)

summary(truncated_unimodel)

#Truncated Model for After Discharge 

#Hospital Data for Discharge 

path= 'Z:/data/RData'
file12 = sprintf('Z:/data/RData/hosp_08to15.Rdata', path)
load(file12)

#Getting Discharge Dates for HE patients

he_only <- subset(he_data, he_data$true_discharge_date != "")

startandend_discharge <- dplyr::select(he_only, c("id","id_number","study_start","study_end","he_first","true_discharge_date"))

hospital_data_discharge <- merge(hosp, startandend_discharge, by="id")


#Finds number of hospital days after discharge

after_discharge <- subset(hospital_data_discharge, hospital_data_discharge$admit_date > hospital_data_discharge$true_discharge_date)

hospdays_afterdischarge <- after_discharge %>% group_by(id_number) %>% dplyr::summarise(hospdays_afterdischarge = sum(days))


#Merges and subsets to people with hospital days after discharge

hospital_discharge_modeldata <- merge(he_only, hospdays_afterdischarge, by= "id_number", all.x =TRUE)

hospital_discharge <- subset(hospital_discharge_modeldata, !is.na(hospital_discharge_modeldata$hospdays_afterdischarge))

hospital_discharge$true_discharge_date <- ymd(hospital_discharge$true_discharge_date)
hospital_discharge$study_end <- ymd(hospital_discharge$study_end)

#Finds the number personyears after discharge

hospital_discharge$personyear_afterdisch <- (hospital_discharge$study_end- hospital_discharge$true_discharge_date)/365.25


#Changes format so we can use the variables

hospital_discharge$personyear_afterdisch <- as.numeric(hospital_discharge$personyear_afterdisch )

hospital_discharge$hospdays_afterdischarge <- as.numeric(hospital_discharge$hospdays_afterdischarge)




truncated_discharge <- dplyr::select(hospital_discharge, c(1:8,11,16,20:26,32,38,62:67,71,75,76))

#Changes GC to no if no record

truncated_discharge$gc_any[is.na(truncated_discharge$gc_any)] <- 0

#Change Rifax flag to zero if no record of taking rifaximin

truncated_discharge$rifax_user_he[is.na(truncated_discharge$rifax_user_he)] <- 0

#Log personyear for offset

truncated_discharge$log_personyear_afterdisch <- log(truncated_discharge$personyear_afterdisch)

truncated_discharge <- subset(truncated_discharge,truncated_discharge$log_personyear_afterdisch != -Inf)


#Truncated negative binomial for hospital days after discharge

gen.trun(0,"NBI",type= "left", name= "lefttr")

truncated_fullmodel_disch <- gamlss(hospdays_afterdischarge~ factor(ascites_have) + 
                                factor(varices_have) + factor(tips_have) + factor(hecc_have)+ 
                                age + factor(region) + factor(Urban) + factor(race_group) + 
                                factor(sex) + factor(esrd) +  factor(ccif) + factor(gc_any)+ 
                                factor(HepC)+ factor(HepB) + factor(AlcCirr) +
                                factor(NotAlcCirr) + factor(rifax_user_he)+ offset(personyear_afterdisch), family= NBIlefttr,
                              data = truncated_discharge)

summary(truncated_fullmodel_disch )

truncated_unimodel <- gamlss(hospdays_afterdischarge~ factor(rifax_user_he)+ offset(personyear_afterdisch), family= NBIlefttr,
                             data = truncated_discharge)

summary(truncated_unimodel)


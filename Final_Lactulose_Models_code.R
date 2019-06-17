#Lactulose Part

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)


he_data <- fread("Z:/data/devindata/final_dataset")

path= 'Z:/data/RxDates'
file9 = sprintf('Z:/data/RData/he_rx.Rdata',path)
load(file9)

he_lact <- he_rx[med == "lactulose"]

startandend <- dplyr::select(he_data, c("id","id_number","study_start","study_end"))

lact_data <- merge(he_lact, startandend, by="id")

lact_data$he_first <- ymd(lact_data$he_first)
lact_data$study_start <- ymd(lact_data$study_start)
lact_data$study_end <- ymd(lact_data$study_end)

lact_cirr <- subset(lact_data, lact_data$date >= lact_data$study_start & lact_data$date <= lact_data$study_end)

lact_days <- lact_cirr %>% group_by(id_number) %>% dplyr::summarise(lact_days = sum(days), lact_fills = length(days))

he_data_lact <- merge(he_data, lact_days, by="id_number", all.x = TRUE)





he_rx2 <- he_rx[med == "rifaximin"]

rifax_data <- merge(he_rx2, startandend, by="id")

rifax_data$he_first <- ymd(rifax_data$he_first)
rifax_data$study_start <- ymd(rifax_data$study_start)
rifax_data$study_end <- ymd(rifax_data$study_end)

rifax_cirr <- subset(rifax_data, rifax_data$date >= rifax_data$study_start & rifax_data$date <= rifax_data$study_end)

rifax_fills <- rifax_cirr %>% group_by(id_number) %>% dplyr::summarise(rifax_fills = length(days))

he_data_lactrif <- merge(he_data_lact, rifax_fills, by="id_number", all.x = TRUE)

#Survival Curves of Medication-defined vs Code-defined

library(survminer)

he_data_lactrif$personyear <- as.numeric(he_data_lactrif$personyear)

#low <- subset(he_data_lactrif, he_data_lactrif$personyear <= 0 )

he_patients <- subset(he_data_lactrif, he_data_lactrif$personyear_he >0 & (he_data_lactrif$he==TRUE | (he_data_lactrif$rifax_days >= 30 | he_data_lactrif$lact_days >= 30)))

he_patients$norifaxlact[(he_patients$rifax_days < 30| is.na(he_patients$rifax_days)) | (he_patients$lact_days < 30 | is.na(he_patients$lact_days))] <- 1

he_patients$norifaxlact[he_patients$rifax_days >= 30 | he_patients$lact_days >= 30] <- 0



med_vs_code <- survfit(Surv(personyear_he,death_censor)~norifaxlact,  type="kaplan-meier", conf.type="log", data= he_patients)


ggsurvplot(med_vs_code, censor = FALSE,  title = "Survival of HE Patients: Diagnosed by code or Medication", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_patients, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Rifax/Lactulose User", "Not Rifax/Lactulose User"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Rifax/Lactulose User", "Not Rifax/Lactulose User"), xlab = "Time (Years)") 

survdiff(Surv(personyear_he,death_censor)~norifaxlact, data=he_patients)



#For Table

he_data_lactrif$rifax_fills[is.na(he_data_lactrif$rifax_fills)] <- 0
he_data_lactrif$lact_fills[is.na(he_data_lactrif$lact_fills)] <- 0

he_data_withpy <- subset(he_data_lactrif, he_data_lactrif$personyear_he>0)

he_data_rif <- subset(he_data_withpy, he_data_withpy$rifax_fills>0)

he_data_lact <- subset(he_data_withpy, he_data_withpy$lact_fills>0)

he_data_rif_only <- subset(he_data_rif, he_data_rif$lact_fills == 0)

he_data_lact_only <- subset(he_data_lact, he_data_lact$rifax_fills == 0)

he_data_both <- subset(he_data_withpy, he_data_withpy$rifax_fills>0 & he_data_withpy$lact_fills>0)



overall_rifax_fill_with <- quantile(he_data_rif$rifax_fills/he_data_rif$personyear_he, c(0.25, 0.5, 0.75))

rifax_only_fill <- quantile(he_data_rif_only$rifax_fills/he_data_rif_only$personyear_he, c(0.25, 0.5, 0.75))

rifax_both_fill <- quantile(he_data_both$rifax_fills/he_data_both$personyear_he, c(0.25, 0.5, 0.75))


overall_lact_fill_with <- quantile(he_data_lact$lact_fills/he_data_lact$personyear_he, c(0.25, 0.5, 0.75))

lact_only_fill <- quantile(he_data_lact_only$lact_fills/he_data_lact_only$personyear_he, c(0.25, 0.5, 0.75))

lact_both_fill <- quantile(he_data_both$lact_fills/he_data_both$personyear_he, c(0.25, 0.5, 0.75))


#Hospital Days

hospital_with_rifax_lact <- subset(he_data_lactrif, !is.na(he_data$hospdays_he))

#3 is used both, 2 is used rifaximin only, 1 is used lactulose only, 0 is none

hospital_with_rifax_lact$rif_lact_user[hospital_with_rifax_lact$lact_days>= 30 & hospital_with_rifax_lact$rifax_days>= 30] <- 3

hospital_with_rifax_lact$rif_lact_user[(hospital_with_rifax_lact$lact_days < 30 | is.na(hospital_with_rifax_lact$lact_days)) & hospital_with_rifax_lact$rifax_days>= 30] <- 2

hospital_with_rifax_lact$rif_lact_user[hospital_with_rifax_lact$lact_days >= 30 & (hospital_with_rifax_lact$rifax_days < 30 | is.na(hospital_with_rifax_lact$rifax_days))] <- 1

hospital_with_rifax_lact$rif_lact_user[(hospital_with_rifax_lact$lact_days < 30 | is.na(hospital_with_rifax_lact$lact_days)) & (hospital_with_rifax_lact$rifax_days < 30 | is.na(hospital_with_rifax_lact$rifax_days))] <- 0


hospital_days_modeldataset <- dplyr::select(hospital_with_rifax_lact, c(1:8,11,16,20:26,32,38,63:67,71,78))

hospital_days_modeldataset$log_personyear_he <- log(hospital_days_modeldataset$personyear_he)

hospital_days_modeldataset <- subset(hospital_days_modeldataset, hospital_days_modeldataset$log_personyear_he != -Inf )

hospital_days_modeldataset$gc_any[is.na(hospital_days_modeldataset$gc_any)] <- 0

hospital_days_modeldataset$race_group <- factor(hospital_days_modeldataset$race_group, levels = c("White","Black", "Other"))



library(gamlss)
library(gamlss.tr)

#Creates Zero-truncated Negative Binomial distribution used for models

gen.trun(0,"NBI",type= "left", name= "lefttr")

truncated_fullmodel <- gamlss(hospdays_he~ factor(ascites_have) + 
                                factor(varices_have) + factor(tips_have) + factor(hecc_have)+ 
                                age + factor(region) + factor(Urban) + factor(race_group) + 
                                factor(sex) + factor(esrd) +  factor(ccif) + factor(gc_any)+ 
                                factor(HepC)+ factor(HepB) + factor(AlcCirr) +
                                factor(NotAlcCirr) + factor(rif_lact_user)+ offset(log_personyear_he), family= NBIlefttr,
                              data = hospital_days_modeldataset)

summary(truncated_fullmodel)

truncated_unimodel <- gamlss(hospdays_he~ factor(rifax_user_he)+ offset(log_personyear_he), family= NBIlefttr,
                             data = hospital_days_modeldataset)

summary(truncated_unimodel)

#Readmission Model

with_readmission <- subset(he_data_lactrif, !is.na(he_data_lactrif$number_of_readmissions_he))


with_readmission$rif_lact_user[with_readmission$lact_days>= 30 & with_readmission$rifax_days>= 30] <- 3

with_readmission$rif_lact_user[(with_readmission$lact_days < 30 | is.na(with_readmission$lact_days)) & with_readmission$rifax_days>= 30] <- 2

with_readmission$rif_lact_user[with_readmission$lact_days >= 30 & (with_readmission$rifax_days < 30 | is.na(with_readmission$rifax_days))] <- 1

with_readmission$rif_lact_user[(with_readmission$lact_days < 30 | is.na(with_readmission$lact_days)) & (with_readmission$rifax_days < 30 | is.na(with_readmission$rifax_days))] <- 0

with_readmission$gc_any[is.na(with_readmission$gc_any)] <- 0

with_readmission$log_personyear <- log(with_readmission$personyear_he)

with_readmission <- subset(with_readmission, with_readmission$log_personyear > -Inf)
with_readmission$rifax_user_he[is.na(with_readmission$rifax_user_he)] <- 0

library(pscl)

readmission_model_zeroinf_lactrif <- zeroinfl(number_of_readmissions_he ~ factor(ascites_have) + factor(varices_have) + factor(tips_have) + factor(hecc_have) +
                                        age + factor(region) + factor(Urban) + factor(race_group)+ 
                                        factor(sex) + factor(esrd) + factor(ccif)+
                                        factor(HepC)+ factor(HepB) + factor(AlcCirr)+ factor(NotAlcCirr) +factor(gc_any) +factor(rif_lact_user)+ offset(log_personyear), dist = "negbin", data = with_readmission)

summary(readmission_model_zeroinf_lactrif)

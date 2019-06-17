#Final Survival Model

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)
library(survminer)

he_data <- fread("Z:/data/devindata/final_dataset")

#Changes into usable date format

he_data$he_first <- ymd(he_data$he_first)
he_data$study_end <- ymd(he_data$study_end)

he_data$race_group <- factor(he_data$race_group, levels = c("White","Black", "Other"))


he_only_data <- subset(he_data, !is.na(he_data$he_first))

he_data2 <- subset(he_data, he_data$study_end > he_data$he_first & !is.na(he_data$he_first))

#Changes into usable date format

he_data2$study_start <- ymd(he_data2$study_start)
he_data2$study_end <- ymd(he_data2$study_end)
he_data2$ascites_start <- ymd(he_data2$ascites_start)
he_data2$varices_start <- ymd(he_data2$varices_start)
he_data2$tips_start <- ymd(he_data2$tips_start)
he_data2$hecc_start <- ymd(he_data2$hecc_start)
he_data2$true_death <- ymd(he_data2$true_death)
he_data2$he_first <- ymd(he_data2$he_first)

he_data2$study_start_numeric <- as.numeric(he_data2$study_start)
he_data2$study_end_numeric <- as.numeric(he_data2$study_end)
he_data2$ascites_start_numeric <- as.numeric(he_data2$ascites_start)
he_data2$varices_start_numeric <- as.numeric(he_data2$varices_start)
he_data2$tips_start_numeric <- as.numeric(he_data2$tips_start)
he_data2$hecc_start_numeric <- as.numeric(he_data2$hecc_start)
he_data2$true_death_numeric <- as.numeric(he_data2$true_death)
he_data2$he_first_numeric <- as.numeric(he_data2$he_first)

#Flags Gastrointestinal Consult or rifax_user as no if no record
he_data2$gc_any[is.na(he_data2$gc_any)] <- 0

he_data2$rifax_user_he[is.na(he_data2$rifax_user_he)] <- 0 

#Separates the data into events based on time-dependent variables and creates the death event

newmodel <- tmerge(data1 = he_data2, data2= he_data2, id = id_number, tstart = he_first_numeric, tstop = study_end_numeric)
newmodel <- tmerge(newmodel,he_data2,id=id_number, ascite = tdc(ascites_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,he_data2,id=id_number, varice = tdc(varices_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,he_data2,id=id_number, tips = tdc(tips_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,he_data2,id=id_number, hecc = tdc(hecc_start_numeric), realcensor= event(true_death_numeric))


#Cox Proportional Hazards Model 
#Doesn't meet proportional hazards assumption but we don't care

modelfull <- coxph(Surv(tstart,tstop,realcensor)~ factor(ascite) + factor(varice) + factor(tips) + factor(hecc) +
                     age + factor(region) + factor(Urban) + factor(race_group)+ 
                     factor(sex) + factor(esrd) + factor(ccif)+
                     factor(HepC)+ factor(HepB) + factor(AlcCirr) + factor(NotAlcCirr) + factor(gc_any) +factor(rifax_user_he),data = newmodel)


summary(modelfull)


model_uni <- coxph(Surv(tstart,tstop,realcensor)~ factor(rifax_user_he),data = newmodel)

summary(model_uni)

#After Discharge, same but for after disharge

he_data$true_discharge_date <- ymd(he_data$true_discharge_date)


discharge_survival <- subset(he_data, !is.na(he_data$true_discharge_date))


discharge_survival$study_start <- ymd(discharge_survival$study_start)
discharge_survival$study_end <- ymd(discharge_survival$study_end)
discharge_survival$ascites_start <- ymd(discharge_survival$ascites_start)
discharge_survival$varices_start <- ymd(discharge_survival$varices_start)
discharge_survival$tips_start <- ymd(discharge_survival$tips_start)
discharge_survival$hecc_start <- ymd(discharge_survival$hecc_start)
discharge_survival$true_death <- ymd(discharge_survival$true_death)
discharge_survival$he_first <- ymd(discharge_survival$he_first)
discharge_survival$true_discharge_date <- ymd(discharge_survival$true_discharge_date)

discharge_survival$study_start_numeric <- as.numeric(discharge_survival$study_start)
discharge_survival$study_end_numeric <- as.numeric(discharge_survival$study_end)
discharge_survival$ascites_start_numeric <- as.numeric(discharge_survival$ascites_start)
discharge_survival$varices_start_numeric <- as.numeric(discharge_survival$varices_start)
discharge_survival$tips_start_numeric <- as.numeric(discharge_survival$tips_start)
discharge_survival$hecc_start_numeric <- as.numeric(discharge_survival$hecc_start)
discharge_survival$true_death_numeric <- as.numeric(discharge_survival$true_death)
discharge_survival$he_first_numeric <- as.numeric(discharge_survival$he_first)
discharge_survival$true_discharge_date_numeric <- as.numeric(discharge_survival$true_discharge_date)

discharge_survival <- subset(discharge_survival, discharge_survival$study_end_numeric > discharge_survival$true_discharge_date_numeric)

discharge_survival$gc_any[is.na(discharge_survival$gc_any)] <- 0

discharge_survival$rifax_user_he[is.na(discharge_survival$rifax_user_he)] <- 0 


newmodel2 <- tmerge(data1 = discharge_survival, data2= discharge_survival, id = id_number, tstart =true_discharge_date_numeric , tstop = study_end_numeric)
newmodel2 <- tmerge(newmodel2,discharge_survival,id=id_number, ascite = tdc(ascites_start_numeric), realcensor= event(true_death_numeric))
newmodel2 <- tmerge(newmodel2,discharge_survival,id=id_number, varice = tdc(varices_start_numeric), realcensor= event(true_death_numeric))
newmodel2 <- tmerge(newmodel2,discharge_survival,id=id_number, tips = tdc(tips_start_numeric), realcensor= event(true_death_numeric))
newmodel2 <- tmerge(newmodel2,discharge_survival,id=id_number, hecc = tdc(hecc_start_numeric), realcensor= event(true_death_numeric))


#Cox Proportional Hazards Model after Discharge

modelfull_disch <- coxph(Surv(tstart,tstop,realcensor)~ factor(ascite) + factor(varice) + factor(tips) + factor(hecc) +
                     age + factor(region) + factor(Urban) + factor(race_group)+ 
                     factor(sex) + factor(esrd) + factor(ccif)+
                     factor(HepC)+ factor(HepB) + factor(AlcCirr) + factor(NotAlcCirr) + factor(gc_any) +factor(rifax_user_he),data = newmodel2)

summary(modelfull_disch)


model_uni_disch <- coxph(Surv(tstart,tstop,realcensor)~ factor(rifax_user_he),data = newmodel2)

summary(model_uni_disch)


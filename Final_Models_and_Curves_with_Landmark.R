#Landmark with Propensity scoring

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)
library(survminer); library(landest)

he_data <- fread("Z:/data/devindata/final_dataset")

#Changing into date format that is suitable to use

he_data$he_first <- ymd(he_data$he_first)
he_data$study_end <- ymd(he_data$study_end)

#Changing race into factor
he_data$race_group <- factor(he_data$race_group, levels = c("White","Black", "Other"))

#Subset to those who have HE event and good study times

he_only_data <- subset(he_data, !is.na(he_data$he_first))

he_data2 <- subset(he_data, he_data$study_end > he_data$he_first & !is.na(he_data$he_first))

#Changing into date format that is suitable to use
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

#Marks as no gc consult if no record

he_data2$gc_any[is.na(he_data2$gc_any)] <- 0


#Extra Code for Getting time of rifaximin or lactulose
#Not Used

# #Getting Time of Rifaximin 
# path= 'Z:/data/RxDates'
# file9 = sprintf('Z:/data/RData/he_rx.Rdata',path)
# load(file9)
# 
# he_rx2 <- he_rx[med == "rifaximin"]
# he_lact <- he_rx[med == "lactulose"]
# 
# he_data_heonly <- subset(he_data, !is.na(he_data$he_first))
# 
# startandend_he <- dplyr::select(he_data_heonly, c("id","id_number","study_start","study_end","he_first"))
# 
# rifax_data <- merge(he_rx2, startandend_he, by="id")
# 
# rifax_data$he_first <- ymd(rifax_data$he_first)
# rifax_data$study_start <- ymd(rifax_data$study_start)
# rifax_data$study_end <- ymd(rifax_data$study_end)
# 
# rifax_cirr <- subset(rifax_data, rifax_data$date >= rifax_data$he_first & rifax_data$date <= rifax_data$study_end)
# 
# #Finds first rifaximin date
# rifax_days <- rifax_cirr %>% group_by(id_number) %>% dplyr::summarise(rifax_first_date = min(date))
# 
# #Merging in the date
# 
# he_with_date <- merge(he_data2, rifax_days, by="id_number", all.x = TRUE)

#Setting Rifaximin Users based on Landmark date of 6 months

landmark_data <- subset(he_data2, he_with_date$personyear_he > 0.50)

landmark_data$rifax_user_6month[landmark_data$rifax_days >= 30] <- 1
landmark_data$rifax_user_6month[landmark_data$rifax_days < 30 | is.na(landmark_data$rifax_days)] <- 0

table(landmark_data$rifax_user_3month)

#Landmark Model

newmodel <- tmerge(data1 = landmark_data, data2= landmark_data, id = id_number, tstart = he_first_numeric, tstop = study_end_numeric)
newmodel <- tmerge(newmodel,landmark_data,id=id_number, ascite = tdc(ascites_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,landmark_data,id=id_number, varice = tdc(varices_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,landmark_data,id=id_number, tips = tdc(tips_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,landmark_data,id=id_number, hecc = tdc(hecc_start_numeric), realcensor= event(true_death_numeric))


#Cox Proportional Hazards model; with HE patients who lived past 6 months

modelfull <- coxph(Surv(tstart,tstop,realcensor)~ factor(ascite) + factor(varice) + factor(tips) + factor(hecc) +
                     age + factor(region) + factor(Urban) + factor(race_group)+ 
                     factor(sex) + factor(esrd) + factor(ccif)+
                     factor(HepC)+ factor(HepB) + factor(AlcCirr) + factor(NotAlcCirr) + factor(gc_any) +factor(rifax_user_6month),data = newmodel)


summary(modelfull)

model_rifax <- coxph(Surv(tstart,tstop,realcensor)~ factor(rifax_user_3month),data = newmodel)

summary(model_rifax)


#Survival Curves

#Rifaximin 

rifax_curve <- survfit(Surv(personyear_he,death_censor)~ rifax_user_3month,  type="kaplan-meier", conf.type="log", data= landmark_data)

ggsurvplot(rifax_curve, censor = FALSE,  title = "Survival of HE Patients: Rifaximin Users", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= landmark_data, xlim = c(0.5,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Non-Rifaximin Users", "Rifaximin Users"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Non-Rifaximin Users", "Rifaximin Users"), xlab = "Time (Years)") 

survdiff(Surv(personyear_he,death_censor)~ rifax_user_3month, data= landmark_data)


#GC Consult

gc_curve <- survfit(Surv(personyear_he,death_censor)~ gc_any,  type="kaplan-meier", conf.type="log", data= landmark_data)

ggsurvplot(gc_curve, censor = FALSE,  title = "Survival of HE Patients: Gastrointestinal Consult", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= landmark_data, xlim = c(0.5,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("No GC", "Had GC"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("No GC", "Had GC"), xlab = "Time (Years)") 

survdiff(Surv(personyear_he,death_censor)~ gc_any, data= landmark_data)

# #Getting Propensity Weights for Rifaxmin
# 
# propensity_covariates <- dplyr::select(landmark_data,c(7,8,11,16,20:26,63:67))
# 
# propensity_covariates$gc_any[is.na(propensity_covariates$gc_any)] <- 0
# 
# propensity_covariates$esrd <- factor(propensity_covariates$esrd)
# propensity_covariates$sex <- factor(propensity_covariates$sex)
# propensity_covariates$Urban <- factor(propensity_covariates$Urban)
# propensity_covariates$region <- factor(propensity_covariates$region)
# propensity_covariates$race_group <- factor(propensity_covariates$race_group)
# propensity_covariates$ccif <- factor(propensity_covariates$ccif)
# propensity_covariates$gc_any <- factor(propensity_covariates$gc_any)
# 
# #model.matrix()
# 
# propensity_weight <- ps.wgt.fun(treat=landmark_data$rifax_user_3month,cov.for.ps = model.matrix(~.,propensity_covariates)[,-1])
# 
# landmark_test <- delta.iptw.km(tl= landmark_data$personyear_he, dl = landmark_data$death_censor, treat = landmark_data$rifax_user_3month, ps.weights = propensity_weight)
# 
# summary(landmark_test)
# 
# landmark_test$S.estimate.1
# 
# 
# landmark_test2 <- delta.iptw.km(tl= landmark_data$personyear_he, tt= 0.5,dl = landmark_data$death_censor, treat = landmark_data$rifax_user_3month, cov.for.ps = model.matrix(~.,propensity_covariates)[,-1])
# 
# landmark_test2
# 
# delta.land.obs(tl= landmark_data$personyear_he, dl = landmark_data$death_censor, treat = landmark_data$rifax_user_3month, tt = 2, z.cov = model.matrix(~.,propensity_covariates)[,-1],  landmark = 0.25, cov.for.ps = model.matrix(~.,propensity_covariates)[,-1]) 
# 
# delta.land.obs(tl= landmark_data$personyear_he, dl = landmark_data$death_censor, treat = landmark_data$rifax_user_3month, tt = 1,  landmark = 0.25, ps.weights = propensity_weight ) 
# 
# 
# surv.iptw.km(tl= landmark_data$personyear_he, dl = landmark_data$death_censor, ps.weights = propensity_weight)
# 
# surv.land.obs(tl= landmark_data$personyear_he, dl = landmark_data$death_censor,  z.cov = model.matrix(~.,propensity_covariates)[,-1], ps.weights = propensity_weight, tt = 1 )
# 


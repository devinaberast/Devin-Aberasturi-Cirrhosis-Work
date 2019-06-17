#Fine-Gray Modeling, used to analyze different censoring events separately

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)
library(survminer)

he_data <- fread("Z:/data/devindata/final_dataset")

he_data$he_first <- ymd(he_data$he_first)
he_data$study_end <- ymd(he_data$study_end)

he_data$race_group <- factor(he_data$race_group, levels = c("White","Black", "Other"))

#Only Patients with HE and have correct start dates

he_only_data <- subset(he_data, !is.na(he_data$he_first))

he_data2 <- subset(he_data, he_data$study_end > he_data$he_first & !is.na(he_data$he_first))

he_data2$study_start <- ymd(he_data2$study_start)
he_data2$study_end <- ymd(he_data2$study_end)
he_data2$ascites_start <- ymd(he_data2$ascites_start)
he_data2$varices_start <- ymd(he_data2$varices_start)
he_data2$tips_start <- ymd(he_data2$tips_start)
he_data2$hecc_start <- ymd(he_data2$hecc_start)
he_data2$true_death <- ymd(he_data2$true_death)
he_data2$he_first <- ymd(he_data2$he_first)
he_data2$lt_date <- ymd(he_data2$lt_date)

he_data2$study_start_numeric <- as.numeric(he_data2$study_start)
he_data2$study_end_numeric <- as.numeric(he_data2$study_end)
he_data2$ascites_start_numeric <- as.numeric(he_data2$ascites_start)
he_data2$varices_start_numeric <- as.numeric(he_data2$varices_start)
he_data2$tips_start_numeric <- as.numeric(he_data2$tips_start)
he_data2$hecc_start_numeric <- as.numeric(he_data2$hecc_start)
he_data2$true_death_numeric <- as.numeric(he_data2$true_death)
he_data2$he_first_numeric <- as.numeric(he_data2$he_first)
he_data2$lt_date_numeric <- as.numeric(he_data2$lt_date)

he_data2$gc_any[is.na(he_data2$gc_any)] <- 0

he_data2$rifax_user_he[is.na(he_data2$rifax_user_he)] <- 0 

he_data3 <- he_data2

he_data2 <- subset(he_data3, he_data3$personyear>0)


he_data2$rifax_user[is.na(he_data2$rifax_user)] <- 0


#Had to make new event variable; so can separate by censored event

he_data2$event[he_data2$study_end == he_data2$true_death] <- "death"
he_data2$event[he_data2$study_end == he_data2$lt_date] <- "transplant"
he_data2$event[(he_data2$study_end != he_data2$true_death | is.na(he_data2$true_death)) & (he_data2$study_end != he_data2$lt_date | is.na(he_data2$lt_date)) ] <- "censor"

survival_data <- subset(he_data2, he_data2$personyear_he > 0)

#Only want the variables I am using and in right format 

survival_data_covariates <- dplyr::select(survival_data,c(7,8,11,16,20:26,61,63:67))

survival_data_covariates$esrd <- factor(survival_data_covariates$esrd)
survival_data_covariates$sex <- factor(survival_data_covariates$sex)
survival_data_covariates$Urban <- factor(survival_data_covariates$Urban)
survival_data_covariates$region <- factor(survival_data_covariates$region)
survival_data_covariates$race_group <- factor(survival_data_covariates$race_group)
survival_data_covariates$ccif <- factor(survival_data_covariates$ccif)

#TRIAL DATASET
# set.seed(10)
# ftime <- rexp(200)
# fstatus <- sample(0:2,200,replace=TRUE)
# cov <- matrix(runif(600),nrow=200)
# dimnames(cov)[[2]] <- c('x1','x2','x3')
# print(z <- crr(ftime,fstatus,cov))

#Needed for the crr function

ftime <- he_data2$personyear_he
fstatus <- he_data2$event

#Covariate matrix
cov <- model.matrix(~.,survival_data_covariates)[,-1]


### FINE-GRAY MODELS for Death and Transplant ### 
library(cmprsk)
fg_model <- crr(ftime,fstatus,cov,failcode="death")

fg_model_transplant <- crr(ftime,fstatus,cov,failcode="transplant")

summary(fg_model)
summary(fg_model_transplant)






###### STUFF I TRIED BUT DIDNT WORK ####


#Got rid of esrd, tips
pdata <- finegray(Surv(personyear_he, factor(event)) ~  factor(ascites_have), etype = "death",  data = survival_data)

fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ ., weight=fgwt, data=pdata)

newmodel <- tmerge(data1 = he_data2, data2= he_data2, id = id_number, tstart = he_first_numeric, tstop = study_end_numeric)
newmodel <- tmerge(newmodel,he_data2,id=id_number, ascite = tdc(ascites_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,he_data2,id=id_number, varice = tdc(varices_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,he_data2,id=id_number, tips = tdc(tips_start_numeric), realcensor= event(true_death_numeric))
newmodel <- tmerge(newmodel,he_data2,id=id_number, hecc = tdc(hecc_start_numeric), realcensor= event(true_death_numeric))


newmodel$event[newmodel$realcensor == 1] <- 2
newmodel$event[newmodel$realcensor == 0 & newmodel$tstop == newmodel$lt_date_numeric] <- 1
newmodel$event[newmodel$realcensor == 0 & (newmodel$tstop != newmodel$lt_date_numeric | is.na(newmodel$lt_date_numeric))] <- 0
newmodel$etime <- newmodel$tstop-newmodel$tstart

newmodel$rifax_user[is.na(newmodel$rifax_user)] <- 0


#Creates the data for the finegray model
pdata <- finegray(Surv(tstart,tstop, factor(event)) ~ factor(ascite) + factor(varice) + factor(tips) + factor(hecc) +
                    age + factor(region) + factor(Urban) + factor(race_group)+ 
                    factor(sex) + factor(esrd) + factor(ccif)+
                    factor(HepC)+ factor(HepB) + factor(AlcCirr) + factor(NotAlcCirr) + factor(gc_any) + factor(rifax_user),id = id_number,data = newmodel)

fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ ., weight=fgwt, data=pdata)


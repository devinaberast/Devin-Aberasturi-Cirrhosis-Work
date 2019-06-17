#FINAL READMISSION MODEL

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)
library(survminer)


he_data <- fread("Z:/data/devindata/final_dataset")

#Flags GC as no if no record

he_data$gc_any[is.na(he_data$gc_any)] <- 0

#Subset to people with record of admission; either of zero or above
with_readmission <- subset(he_data, !is.na(he_data$number_of_readmissions_he))

#Need log personyear for offset
with_readmission$log_personyear <- log(with_readmission$personyear_he)

#Subsets so have good offset
with_readmission <- subset(with_readmission, with_readmission$log_personyear > -Inf)

#Flags as not rifaximin user if no record

with_readmission$rifax_user_he[is.na(with_readmission$rifax_user_he)] <- 0

#Formats race_group as factor
with_readmission$race_group <- factor(with_readmission$race_group, levels = c("White","Black", "Other"))

#Used these to prove that Zero-Inflated Binomial good for readmission model
hist(with_readmission$number_of_readmissions_he)
plot(density(with_readmission$number_of_readmissions_he), main = "Density of Readmissions")
polygon(density(with_readmission$number_of_readmissions_he), col="red", border="black")


#Different Models that could've been fitted

#Poisson Model with personyear offset

readmission_model <- glm(number_of_readmissions_he ~ factor(ascites_have) + factor(varices_have) + factor(tips_have) + factor(hecc_have) +
                           age + factor(region) + factor(Urban) + factor(race_group)+ 
                           factor(sex) + factor(esrd) + factor(ccif)+
                           factor(HepC)+ factor(HepB) + factor(AlcCirr) + factor(NotAlcCirr)+ factor(gc_any) +factor(rifax_user_he)+ offset(log_personyear), family = "poisson", data = with_readmission)

summary(readmission_model)


#Testing the dispersion of the data

library(AER)

deviance(readmission_model)/readmission_model$df.residual
dispersiontest(readmission_model)

#Negative Binomial Model

readmission_model_negbin <- MASS::glm.nb(number_of_readmissions_he ~ factor(ascites_have) + factor(varices_have) + factor(tips_have) + factor(hecc_have) +
                                           age + factor(region) + factor(Urban) + factor(race_group)+ 
                                           factor(sex) + factor(esrd) + factor(ccif)+
                                           factor(HepC)+ factor(HepB) + factor(AlcCirr)+ factor(NotAlcCirr) + factor(gc_any) +factor(rifax_user_he)+ offset(log_personyear), data = with_readmission)

summary(readmission_model_negbin)

library(pscl)

#Zeroinfalted poisson model 

readmission_model_zeroinf <- zeroinfl(number_of_readmissions_he ~ factor(ascites_have) + factor(varices_have) + factor(tips_have) + factor(hecc_have) +
                                age + factor(region) + factor(Urban) + factor(race_group)+ 
                                factor(sex) + factor(esrd) + factor(ccif)+
                                factor(HepC)+ factor(HepB) + factor(AlcCirr)+ factor(NotAlcCirr) +factor(gc_any) +factor(rifax_user_he)+ offset(log_personyear), dist = "poisson", data = with_readmission)

summary(readmission_model_zeroinf)

#Zero-inflated negative binomial model

readmission_model_zeroinf_negbin <- zeroinfl(number_of_readmissions_he ~ factor(ascites_have) + factor(varices_have) + factor(tips_have) + factor(hecc_have) +
                                        age + factor(region) + factor(Urban) + factor(race_group)+ 
                                        factor(sex) + factor(esrd) + factor(ccif)+
                                        factor(HepC)+ factor(HepB) + factor(AlcCirr)+ factor(NotAlcCirr) + factor(gc_any) +factor(rifax_user_he)+ offset(log_personyear), dist = "negbin", data = with_readmission)

summary(readmission_model_zeroinf_negbin)


#Test to see if zero-inflated negative binomial model versus negative binomial model
#Supports zero-inflated negative binomial better

vuong(readmission_model_negbin,readmission_model_zeroinf_negbin)

#Shows zeroinflated negative binomial has lowest AIC
AIC(readmission_model)
AIC(readmission_model_zeroinf)
AIC(readmission_model_negbin)
AIC(readmission_model_zeroinf_negbin)

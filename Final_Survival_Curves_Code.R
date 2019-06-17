#Survival Curves

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)

library(survminer)

he_data <- fread("Z:/data/devindata/final_dataset")

he_data <- subset(he_data, he_data$personyear > 0) 

#Survival after first Cirrhosis Event

#For HE versus No HE

heandnonhe <- survfit(Surv(personyear,death_censor)~he,  type="kaplan-meier", conf.type="log", data= he_data)

ggsurvplot(heandnonhe, censor = FALSE,  title = "Survival of Cirrhosis Patients: HE and Non-HE", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_data, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Non-HE Patients", "HE Patients"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Non-HE Patients", "HE Patients"), xlab = "Time (Years)") 

#Log-rank test of difference between HE and Non-HE 

survdiff(Surv(personyear,death_censor)~he, data=he_data)


#Finds survival curves for Non-HE, Inpatient HE, and Outpatient HE

he_data$inpatient_outpatient_nonhe[he_data$he == FALSE] <- "No_He"
he_data$inpatient_outpatient_nonhe[he_data$inpatient_flag_he == 1] <- "Inpatient_He"
he_data$inpatient_outpatient_nonhe[he_data$inpatient_flag_he == 0] <- "Outpatient_He"

table(he_data$inpatient_outpatient_nonhe)

heandinpatient <- survfit(Surv(personyear,death_censor)~inpatient_outpatient_nonhe,  type="kaplan-meier", conf.type="log", data= he_data)

ggsurvplot(heandinpatient, censor = FALSE,  title = "Survival of Cirrhosis Patients: Inpatient and Outpatient with HE", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_data, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Inpatient with HE", "No HE", "Outpatient with HE"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Inpatient with HE", "No HE", "Outpatient with HE"), xlab = "Time (Years)") 

#Log-Rank Test between the three groups: Non-HE, Inpatient HE, and Outpatient HE

survdiff(Surv(personyear,death_censor)~inpatient_flag_he, data=he_data)

#### For Rest, Outcome is Survival after first HE event ####
### Only HE patients ###

he_data <- subset(he_data, he_data$personyear_he > 0)

#Plots Survival Curves between Patients with Ascites and without

ascites <- survfit(Surv(personyear_he,death_censor)~ ascites_have, type="kaplan-meier", conf.type="log", data= he_data)

ggsurvplot(ascites, censor = FALSE,  title = "Survival of HE Patients: Ascites", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_data, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Patients Who Never had Ascites", "Patients Who had or Developed Ascites"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Patients Who Never had Ascites", "Patients Who had or Developed Ascites"), xlab = "Time (Years)") 

#Log-Rank Test between Patients with Ascites and without

survdiff(Surv(personyear_he,death_censor)~ascites_have, data=he_data)

#Plots Survival Curve between Patients with TIPS and Without

tips <- survfit(Surv(personyear_he,death_censor)~ tips_have, type="kaplan-meier", conf.type="log", data= he_data)

ggsurvplot(tips, censor = FALSE,  title = "Survival of HE Patients: Tips", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_data, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Patients Who Never had Tips", "Patients Who had or Developed Tips"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Patients Who Never had Tips", "Patients Who had or Developed Tips"), xlab = "Time (Years)") 

#Log-Rank Test Between Patients with TIPS and Without

survdiff(Surv(personyear_he,death_censor)~tips_have, data=he_data)


#Plots Survival Curve Between Patients with Varices and Without

varices <- survfit(Surv(personyear_he,death_censor)~ varices_have, type="kaplan-meier", conf.type="log", data= he_data)

ggsurvplot(varices, censor = FALSE,  title = "Survival of HE Patients: Varices", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_data, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Patients Who Never had Varices", "Patients Who had or Developed Varices"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Patients Who Never had Varices", "Patients Who had or Developed Varices"), xlab = "Time (Years)") 

#Log-Rank Test Between Patients With Varices and without

survdiff(Surv(personyear_he,death_censor)~varices_have, data=he_data)


#Plots Survival Curve Between Patients with HECC and without

hecc <- survfit(Surv(personyear_he,death_censor)~ hecc_have, type="kaplan-meier", conf.type="log", data= he_data)

ggsurvplot(hecc, censor = FALSE,  title = "Survival of HE Patients: Hepatocellular Carcinoma", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_data, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Patients Who Never had HECC", "Patients Who had or Developed HECC"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Patients Who Never had HECC", "Patients Who had or Developed HECC"), xlab = "Time (Years)") 

#Log-Rank Test Between Patients with HECC and Without

survdiff(Surv(personyear_he,death_censor)~hecc_have, data=he_data)

#Plots Survival Curve Over age of 65 

over65 <- survfit(Surv(personyear_he,death_censor)~ over_65, type="kaplan-meier", conf.type="log", data= he_data)

ggsurvplot(over65, censor = FALSE,  title = "Survival of HE Patients: Age 65 and Over", break.time.by = 1, conf.int = TRUE, 
           conf.int.alpha = 0.4,conf.int.style = "ribbon", ggtheme= theme_bw() ,data= he_data, xlim = c(0,5), ylim = c(0,1), axes.offset = TRUE,
           legend.lab = c("Patients Under Age of 65", "Patients over Age of 65"), risk.table=TRUE, risk.table.title = "Number of Patients at Risk", 
           risk.table.y.text= FALSE, risk.table.y.col = "black", ncensor.table = TRUE, legend.labs= c("Patients Under Age of 65", "Patients over Age of 65"), xlab = "Time (Years)") 

#Log-Rank Test Between Patients Over 65 versus patients under 65

survdiff(Surv(personyear_he,death_censor)~ over_65, data=he_data)

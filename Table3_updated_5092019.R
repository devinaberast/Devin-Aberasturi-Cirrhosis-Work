library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)

he_data<- fread("Z:/data/devindata/final_dataset")

he_data$hosp

value <- he_data %>%  group_by(factor(he)) %>%  summarise_at(vars(hospdays),funs(quantile(., c(0.5), na.rm=TRUE)))

he_data2 <- subset(he_data, he_data$personyear >0)

he_data2$lt_date <- ymd(he_data2$lt_date)

he_data2$lt_flag[is.na(he_data2$lt_date)] <- 0

he_data2$lt_flag[!is.na(he_data2$lt_date)& he_data2$lt_date > he_data2$study_end] <- 0

he_data2$lt_flag[!is.na(he_data2$lt_date)& he_data2$lt_date <= he_data2$study_end] <- 1

he_data2$hospdays[is.na(he_data2$hospdays)] <- 0

### Table 3 ### 


#Gender

gender_2year <- survfit(Surv(personyear,death_censor)~ sex,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(gender_2year, times=3)

table(he_data2$sex)

male_data <- subset(he_data2, he_data2$sex == "Male")

female_data <- subset(he_data2, he_data2$sex == "Female")

male_hospday <- quantile(male_data$hospdays/male_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

male_hospday

female_hospday <- quantile(female_data$hospdays/female_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

female_hospday

#Race

race_2year <- survfit(Surv(personyear,death_censor)~ race_group,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(race_2year, times=3)

table(he_data2$race_group)

white_data <- subset(he_data2, he_data2$race_group == "White")
black_data <- subset(he_data2, he_data2$race_group == "Black")
other_data <- subset(he_data2, he_data2$race_group == "Other")


white_hospday <- quantile(white_data$hospdays/white_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

white_hospday

black_hospday <- quantile(black_data$hospdays/black_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

black_hospday

other_hospday <- quantile(other_data$hospdays/other_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

other_hospday

#Age Group

he_data2$agegroup[he_data2$age >= 19 & he_data2$age <= 50] <- 1
he_data2$agegroup[he_data2$age >= 50 & he_data2$age < 55] <- 2
he_data2$agegroup[he_data2$age >= 55 & he_data2$age < 60] <- 3
he_data2$agegroup[he_data2$age >= 60 & he_data2$age < 65] <- 4
he_data2$agegroup[he_data2$age >= 65 & he_data2$age < 70] <- 5
he_data2$agegroup[he_data2$age >= 70 & he_data2$age < 75] <- 6
he_data2$agegroup[he_data2$age >= 75 & he_data2$age < 80] <- 7 
he_data2$agegroup[he_data2$age >= 80] <- 8

agegroup_2year <- survfit(Surv(personyear,death_censor)~ agegroup,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(agegroup_2year, times=3)

agegroup1_data <- subset(he_data2, he_data2$agegroup == 1)
agegroup2_data <- subset(he_data2, he_data2$agegroup == 2)
agegroup3_data <- subset(he_data2, he_data2$agegroup == 3)
agegroup4_data <- subset(he_data2, he_data2$agegroup == 4)
agegroup5_data <- subset(he_data2, he_data2$agegroup == 5)
agegroup6_data <- subset(he_data2, he_data2$agegroup == 6)
agegroup7_data <- subset(he_data2, he_data2$agegroup == 7)
agegroup8_data <- subset(he_data2, he_data2$agegroup == 8)

agegroup1_hospday <- quantile(agegroup1_data$hospdays/agegroup1_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup1_hospday

agegroup2_hospday <- quantile(agegroup2_data$hospdays/agegroup2_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup2_hospday

agegroup3_hospday <- quantile(agegroup3_data$hospdays/agegroup3_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup3_hospday

agegroup4_hospday <- quantile(agegroup4_data$hospdays/agegroup4_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup4_hospday

agegroup5_hospday <- quantile(agegroup5_data$hospdays/agegroup5_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup5_hospday

agegroup6_hospday <- quantile(agegroup6_data$hospdays/agegroup6_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup6_hospday

agegroup7_hospday <- quantile(agegroup7_data$hospdays/agegroup7_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup7_hospday

agegroup8_hospday <- quantile(agegroup8_data$hospdays/agegroup8_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

agegroup8_hospday



#Comorbidity

comorbidity_2year <- survfit(Surv(personyear,death_censor)~ ccif,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(comorbidity_2year, times=3)

table(he_data2$ccif)

como0_data <- subset(he_data2, he_data2$ccif == 0)
como1_data <- subset(he_data2, he_data2$ccif == 1)
como2_data <- subset(he_data2, he_data2$ccif == 2)
como3_data <- subset(he_data2, he_data2$ccif == "3+")

como0_hospday <- quantile(como0_data$hospdays/como0_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

como0_hospday

como1_hospday <- quantile(como1_data$hospdays/como1_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

como1_hospday

como2_hospday <- quantile(como2_data$hospdays/como2_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

como2_hospday

como3_hospday <- quantile(como3_data$hospdays/como3_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

como3_hospday



#Alcohlic cirrhosis

alc_2year <- survfit(Surv(personyear,death_censor)~ AlcCirr,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(alc_2year, times=3)

table(he_data2$AlcCirr)

alc_data <- subset(he_data2, he_data2$AlcCirr == TRUE)

alc_hospday <- quantile(alc_data$hospdays/alc_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

alc_hospday

#Hepatitis C

hepc_2year <-  survfit(Surv(personyear,death_censor)~ HepC,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(hepc_2year, times=3)

table(he_data2$HepC)

hepc_data <- subset(he_data2, he_data2$HepC == TRUE)

hepc_hospday <- quantile(hepc_data$hospdays/hepc_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

hepc_hospday

#Hepatitis B

hepb_2year <- survfit(Surv(personyear,death_censor)~ HepB,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(hepb_2year, times=3)

table(he_data2$HepB)

hepB_data <- subset(he_data2, he_data2$HepB == TRUE)

hepB_hospday <- quantile(hepB_data$hospdays/hepB_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

hepB_hospday

#Non-Alc 

nonalc_2year <-  survfit(Surv(personyear,death_censor)~ NotAlcCirr,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(nonalc_2year , times=3)

table(he_data2$NotAlcCirr)

NotAlc_data <- subset(he_data2, he_data2$NotAlcCirr == TRUE)

NotAlc_hospday <- quantile(NotAlc_data$hospdays/NotAlc_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

NotAlc_hospday


#Varices


vari_2year <- survfit(Surv(personyear,death_censor)~ varices_have,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(vari_2year, times=3)

table(he_data2$varices_have)

varices_data <- subset(he_data2, he_data2$varices_have == 1)


varices_hospday <- quantile(varices_data$hospdays/varices_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

varices_hospday


#Ascites

ascites_2year <- survfit(Surv(personyear,death_censor)~ ascites_have,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(ascites_2year, times=3)

table(he_data2$ascites_have)

ascites_data <- subset(he_data2, he_data2$ascites_have == 1)

ascites_hospday <- quantile(ascites_data$hospdays/ascites_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

ascites_hospday

#Tips

tips_2year <- survfit(Surv(personyear,death_censor)~ tips_have,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(tips_2year, times=3)

table(he_data2$tips_have)

tips_data <- subset(he_data2, he_data2$tips_have == 1)

tips_hospday <- quantile(tips_data$hospdays/tips_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

tips_hospday
#Region

region_2year <- survfit(Surv(personyear,death_censor)~ region,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(region_2year , times=3)

table(he_data2$region)

midwest_data <- subset(he_data2, he_data2$region == "Midwest")
northeast_data <- subset(he_data2, he_data2$region == "Northeast")
south_data <- subset(he_data2, he_data2$region == "South")
west_data <- subset(he_data2, he_data2$region == "West")

midwest_hospday <- quantile(midwest_data$hospdays/midwest_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

midwest_hospday

northeast_hospday <- quantile(northeast_data$hospdays/northeast_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

northeast_hospday

south_hospday <- quantile(south_data$hospdays/south_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

south_hospday

west_hospday <- quantile(west_data$hospdays/west_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

west_hospday

#Urban

urban_2year <- survfit(Surv(personyear,death_censor)~ Urban,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(urban_2year , times=3)

table(he_data2$Urban)

urban_data <- subset(he_data2, he_data2$Urban == "Urban")
rural_data <- subset(he_data2, he_data2$Urban == "Rural")

urban_hospday <- quantile(urban_data$hospdays/urban_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

urban_hospday

rural_hospday <- quantile(rural_data$hospdays/rural_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

rural_hospday

#ESRD

esrd_2year <- survfit(Surv(personyear,death_censor)~ esrd,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(esrd_2year , times=3)

table(he_data2$esrd)

esrdyes_data <- subset(he_data2, he_data2$esrd == "Yes")
esrdno_data <- subset(he_data2, he_data2$esrd == "No")

esrdyes_hospday <- quantile(esrdyes_data$hospdays/esrdyes_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

esrdyes_hospday

esrdno_hospday <- quantile(esrdno_data$hospdays/esrdno_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

esrdno_hospday

#Disability


disab_2year <- survfit(Surv(personyear,death_censor)~ disability,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(disab_2year , times=3) 

table(he_data2$disability)

disabilityyes_data <- subset(he_data2, he_data2$disability == "Yes")
disabilityno_data <- subset(he_data2, he_data2$disability == "No")


disabilityyes_hospday <- quantile(disabilityyes_data$hospdays/disabilityyes_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

disabilityyes_hospday

disabilityno_hospday <- quantile(disabilityno_data$hospdays/disabilityno_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

disabilityno_hospday


#Medicaid coinsurance

medicaid_2year <- survfit(Surv(personyear,death_censor)~ caid_start,  type="kaplan-meier", conf.type="log", data=he_data2)

summary(medicaid_2year, times=3) 

table(he_data2$caid_start)

caidnone_data <- subset(he_data2, he_data2$caid_start == "none")
caidpart_data <- subset(he_data2, he_data2$caid_start == "part")
caidfull_data <- subset(he_data2, he_data2$caid_start == "full")

caidnone_hospday <- quantile(caidnone_data$hospdays/caidnone_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

caidnone_hospday

caidpart_hospday <- quantile(caidpart_data$hospdays/caidpart_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

caidpart_hospday

caidfull_hospday <- quantile(caidfull_data$hospdays/caidfull_data$personyear, c(0.25, 0.5, 0.75), na.rm=TRUE)

caidfull_hospday

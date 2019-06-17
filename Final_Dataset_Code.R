#Final Version of Hospital Dataset 

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(haven);library(dplyr)

#Loads Cohort Data from Group's files; filename is he

path= 'Z:/data/RData'
file = sprintf('%s/management_cohort.Rdata', path)
load(file)

#Had to do this since file was in weird format, also took out columns I never use

he_data_orig <- dplyr::select(he, c(1,7:15,17,18, 24,25,28,29,30,32,35:38,41,44,49,58,61,64,67,76,80))

#Keeps only distinct Ids

he_data_orig_distinct <- distinct(he_data_orig, id, .keep_all = TRUE)

#Gives unique ID for each person; Better merging if you use number instead of id name

he_data_orig_distinct$id_number <- ave(he_data_orig_distinct$id,FUN= seq_along)

identification_data <- dplyr::select(he_data_orig_distinct, c("id","id_number"))

#Real Deaths from Cohort, code for getting real deaths is at the end in comments
#Other death was messed up, didn't show enough deaths

deaths <- fread("Z:/data/devindata/realdeaths")
deaths$id <- deaths$BENE_ID
deaths$true_death <- deaths$BENE_DEATH_DT

#Selecting Columns I want
deaths2 <- dplyr::select(deaths,c(3:4))

#Merging in the deaths into HE data
he_data <- merge(he_data_orig_distinct, deaths2,by="id", all.x = TRUE)


#Back-dates cirrhosis time to initial coverage if within 180 days
he_data$cirr_start_true[he_data$no_cirr <= 180]<- as.character(he_data$covstart[he_data$no_cirr <= 180])
he_data$cirr_start_true[he_data$no_cirr > 180] <- as.character(he_data$cirr_first[he_data$no_cirr > 180])

#Changing format into date format that can be used; used throughout
he_data$cirr_start_true <- ymd(he_data$cirr_start_true)
he_data$end_date <- ymd(he_data$end_date)
he_data$lt_date <- ymd(he_data$lt_date)
he_data$true_death <- ymd(he_data$true_death)

#Creating the study start and end date
he_data$study_start <- he_data$cirr_start_true
he_data$study_end <- pmin(he_data$end_date, he_data$true_death, he_data$lt_date, na.rm = TRUE)

#Hospital Data; filename is hosp
path= 'Z:/data/RData'
file12 = sprintf('Z:/data/RData/hosp_08to15.Rdata', path)
load(file12)

#Information on start and end time  after first cirrhosis 

startandend <- dplyr::select(he_data, c("id","id_number","study_start","study_end"))


#Merges in information about start and end time for patient

hospital_data <- merge(hosp,startandend, by="id")

hospital_data$admit_date <- ymd(hospital_data$admit_date)
hospital_data$disch_date <- ymd(hospital_data$disch_date)
hospital_data$study_start <- ymd(hospital_data$study_start)
hospital_data$study_end <- ymd(hospital_data$study_end)

#Only Hospital Dates within study period

hospital_cirr <- subset(hospital_data, hospital_data$admit_date>=hospital_data$study_start &  hospital_data$admit_date <= hospital_data$study_end)

#Aggregates and Calculates number of hospital days

hospdays_cirr <- hospital_cirr %>% group_by(id_number) %>% dplyr::summarise(hospdays = sum(days))

#Finds the number of hospital days after first HE event


he_data_heonly <- subset(he_data, !is.na(he_data$he_first))

startandend_he <- dplyr::select(he_data_heonly, c("id","id_number","study_start","study_end","he_first"))

hospital_data_he <- merge(hosp, startandend_he, by="id")

hospital_he <- subset(hospital_data_he, hospital_data_he$admit_date >=hospital_data_he$he_first &  hospital_data_he$admit_date <= hospital_data_he$study_end)

#Aggregates and Calculates number of hospital days after HE 

hospdays_he <- hospital_he %>% group_by(id_number) %>% dplyr::summarise(hospdays_he = sum(days))


#Fall and fractures data; datafile is frac

file5 = sprintf('%s/falls_fracs.RData', path)
load(file5)

#Looks to see the cases are 30 days apart 

df_falls_fracs[ , delta := c(Inf, diff(date)), id]
df_falls_fracs[ , seq := 1:.N, id]
df_falls_fracs[ ,  ep := cumsum(delta > 30), id]

fracture_data <- merge(df_falls_fracs, startandend, by="id")

fracture_data$date <- ymd(fracture_data$date)
fracture_data$studystart  <- ymd(fracture_data$studystart)
fracture_data$studyend<- ymd(fracture_data$studyend)

fracture_cirr <- subset(fracture_data, fracture_data$date >= fracture_data$study_start & fracture_data$date <= fracture_data$study_end )

#Calculates number of days in hospital due for fracture or falls

fracture_days <- fracture_cirr %>% group_by(id_number) %>% dplyr::summarise(fracture_times = max(ep))


#Fall and Fracture, after HE

fracture_data_he <- merge(df_falls_fracs, startandend_he, by="id")

fracture_data_he$date <- ymd(fracture_data_he$date)
fracture_data_he$he_first  <- ymd(fracture_data_he$he_first)
fracture_data_he$studyend<- ymd(fracture_data_he$studyend)

fracture_he <- subset(fracture_data_he, fracture_data_he$date >= fracture_data_he$he_first & fracture_data_he$date <= fracture_data_he$study_end )

fracture_days_he <- fracture_he %>% group_by(id_number) %>% dplyr::summarise(fracture_times_he = max(ep))

#Benzo data

path= 'Z:/data/RxDates'
file6 = sprintf('%s/benzo.Rdata', path)
load(file6)

benzo_data <- merge(benzo, startandend, by = "id") 

#Finds number of recorded days of taking benzo

benzo_cirr <- subset(benzo_data, benzo_data$date >= benzo_data$study_start & benzo_data$date <= benzo_data$study_end)

benzo_days <- benzo_cirr %>% group_by(id_number) %>% dplyr::summarise(benzo_days = sum(days))


#Finds number of recorded days of taking benzo after first event of HE

benzo_data_he <- merge(benzo,startandend_he, by = "id")

benzo_data_he$he_first <- ymd(benzo_data_he$he_first)
benzo_data_he$study_start <- ymd(benzo_data_he$study_start)
benzo_data_he$study_end <- ymd(benzo_data_he$study_end)

benzo_he <- subset(benzo_data_he, benzo_data_he$date >= benzo_data_he$he_first & benzo_data_he$date <= benzo_data_he$study_end)

benzo_days_he <- benzo_he %>% group_by(id_number) %>% dplyr::summarise(benzo_days_he = sum(days))

#Opiate Data

path= 'Z:/data/RxDates'
file7 = sprintf('%s/opiate.Rdata', path)
load(file7)

opiate_data <- merge(opiate, startandend, by = "id") 

#Finds number of recorded days of taking opiate 

opiate_cirr <- subset(opiate_data, opiate_data$date >= opiate_data$study_start & opiate_data$date <= opiate_data$study_end)

opiate_days <- opiate_cirr %>% group_by(id_number) %>% dplyr::summarise(opiate_days = sum(days))


#Finds number of recorded days of taking opiate after first event of HE

opiate_data_he <- merge(opiate,startandend_he, by = "id")

opiate_data_he$he_first <- ymd(opiate_data_he$he_first)
opiate_data_he$study_start <- ymd(opiate_data_he$study_start)
opiate_data_he$study_end <- ymd(opiate_data_he$study_end)

opiate_he <- subset(opiate_data_he, opiate_data_he$date >= opiate_data_he$he_first & opiate_data_he$date <= opiate_data_he$study_end)

opiate_days_he <- opiate_he %>% group_by(id_number) %>% dplyr::summarise(opiate_days_he = sum(days))


#Proton Pump Inhibitor, PPI Data

path= 'Z:/data/RxDates'
file8 = sprintf('%s/ppi.Rdata', path)
load(file8)

ppi_data <- merge(ppi, startandend, by = "id") 

#Finds number of recorded days of taking PPI

ppi_cirr <- subset(ppi_data, ppi_data$date >= ppi_data$study_start & ppi_data$date <= ppi_data$study_end)

ppi_days <- ppi_cirr %>% group_by(id_number) %>% dplyr::summarise(ppi_days = sum(days))


#Finds number of recorded days of taking PPI after first event of HE

ppi_data_he <- merge(ppi,startandend_he, by = "id")

ppi_data_he$he_first <- ymd(ppi_data_he$he_first)
ppi_data_he$study_start <- ymd(ppi_data_he$study_start)
ppi_data_he$study_end <- ymd(ppi_data_he$study_end)

ppi_he <- subset(ppi_data_he, ppi_data_he$date >= ppi_data_he$he_first & ppi_data_he$date <= ppi_data_he$study_end)

ppi_days_he <- ppi_he %>% group_by(id_number) %>% dplyr::summarise(ppi_days_he = sum(days))

#Rifaximin and Lactulose Data

path= 'Z:/data/RxDates'
file9 = sprintf('Z:/data/RData/he_rx.Rdata',path)
load(file9)

#Two datasets, one for rifaximin, other for lactulose
he_rx2 <- he_rx[med == "rifaximin"]
he_lact <- he_rx[med == "lactulose"]

rifax_data <- merge(he_rx2, startandend, by="id")


#Finds number of recorded days of taking Rifaximin

rifax_data$he_first <- ymd(rifax_data$he_first)
rifax_data$study_start <- ymd(rifax_data$study_start)
rifax_data$study_end <- ymd(rifax_data$study_end)

rifax_cirr <- subset(rifax_data, rifax_data$date >= rifax_data$study_start & rifax_data$date <= rifax_data$study_end)

rifax_days <- rifax_cirr %>% group_by(id_number) %>% dplyr::summarise(rifax_days = sum(days))

#Finds number of recorded days of taking Rifaximin after first event of HE

rifax_data_he <- merge(he_rx2, startandend_he, by= "id")

rifax_data_he$he_first <- ymd(rifax_data_he$he_first)
rifax_data_he$study_start <- ymd(rifax_data_he$study_start)
rifax_data_he$study_end <- ymd(rifax_data_he$study_end)

rifax_he <- subset(rifax_data_he, rifax_data_he$date >= rifax_data_he$he_first & rifax_data_he$date <= rifax_data_he$study_end)

rifax_days_he <- rifax_he %>% group_by(id_number) %>% dplyr::summarise(rifax_days_he = sum(days))

#Inpatient and Outpatient

path= 'Z:/data/RData'
file16 = sprintf('Z:/data/RData/CirrhosisIDs_25Apr18.Rdata', path)
load(file16)

cirr2 <- plyr::rename(cirr,c(BENE_ID = "id", N = "number_of_procedures"))

inpatient <- subset(cirr2, cirr$source == "IP")

inpatient_data <- merge(inpatient, startandend, by="id")

inpatient_data$study_start <- ymd(inpatient_data$study_start)
inpatient_data$study_end <- ymd(inpatient_data$study_end)

inpatient_cirr <- subset(inpatient_data,inpatient_data$year >= year(inpatient_data$study_start) & inpatient_data$year <= year(inpatient_data$study_end) )

#Number of Inpatient Procedures, used for defining inpatient vs outpatient

inpatient_number <- inpatient_cirr %>% group_by(id_number) %>% dplyr::summarise(inpatient_num_procedures = sum(number_of_procedures))

#Inpatient after HE

inpatient_data_he <- merge(inpatient, startandend_he, by="id")

inpatient_data_he$he_first <- ymd(inpatient_data_he$he_first)
inpatient_data_he$study_end <- ymd(inpatient_data_he$study_end)

inpatient_he <- subset(inpatient_data_he,inpatient_data_he$year >= year(inpatient_data_he$he_first) & inpatient_data_he$year <= year(inpatient_data_he$study_end) )

#Number of Inpatient procedures after first event of HE

inpatient_number_he <- inpatient_he %>% group_by(id_number) %>% dplyr::summarise(inpatient_num_procedures_he = sum(number_of_procedures))


#Outpatient 

outpatient <- subset(cirr2, cirr$source == "OP" | cirr$source == "B")

outpatient_data <- merge(outpatient, startandend, by="id")

outpatient_data$study_start <- ymd(outpatient_data$study_start)
outpatient_data$study_end <- ymd(outpatient_data$study_end)

outpatient_cirr <- subset(outpatient_data,outpatient_data$year >= year(outpatient_data$study_start) & outpatient_data$year <= year(outpatient_data$study_end) )

#Number of Outpatient procedures

outpatient_number <- outpatient_cirr %>% group_by(id_number) %>% dplyr::summarise(outpatient_num_procedures = sum(number_of_procedures))

#After HE

outpatient_data_he <- merge(outpatient, startandend_he, by="id")

outpatient_data_he$he_first <- ymd(outpatient_data_he$he_first)
outpatient_data_he$study_end <- ymd(outpatient_data_he$study_end)

outpatient_he <- subset(outpatient_data_he,outpatient_data_he$year >= year(outpatient_data_he$he_first) & outpatient_data_he$year <= year(outpatient_data_he$study_end) )

#Number of outpatient procedures after first event of HE

outpatient_number_he <- outpatient_he %>% group_by(id_number) %>% dplyr::summarise(outpatient_num_procedures_he = sum(number_of_procedures))


#Merging in the information

#Merging Dataset, so not to mess with original

he_data3 <- he_data

#Hospital Days

# hospdays_cirr = hospital days since first cirrhosis event
# hospdays_he = hospital days since first HE event

he_data3 <- merge(he_data3, hospdays_cirr, by= "id_number", all.x= TRUE)
he_data3 <- merge(he_data3, hospdays_he, by= "id_number", all.x = TRUE)


#Falls and Fractures

# fracture_times = number of times they had fracture since first cirrhosis event
# fracture_times_he = number of times they had fractures since first HE event

he_data3 <- merge(he_data3, fracture_days, by= "id_number", all.x=TRUE)
he_data3 <- merge(he_data3, fracture_days_he, by= "id_number", all.x=TRUE)

#Benzo 

# benzo_days = number of days taking benzo since first cirrhosis event
# benzo_days_he = number of days taking benzo since first HE event

he_data3 <- merge(he_data3, benzo_days, by= "id_number", all.x=TRUE)
he_data3 <- merge(he_data3, benzo_days_he, by= "id_number", all.x=TRUE)

#Opiate

# opiate_days = number of days taking opiate since first cirrhosis event
# opiate_days_he = number of days taking opiate since first HE event

he_data3 <- merge(he_data3, opiate_days, by= "id_number", all.x=TRUE)
he_data3 <- merge(he_data3, opiate_days_he, by= "id_number", all.x=TRUE)

#PPI

# ppi_days = number of days taking ppi since first cirrhosis event
# ppi_days_he = number of days taking ppi since first HE event

he_data3 <- merge(he_data3, ppi_days, by= "id_number", all.x=TRUE)
he_data3 <- merge(he_data3, ppi_days_he, by= "id_number", all.x=TRUE)

#Rifaximin

# rifax_days = number of days taking rifax since first cirrhosis event
# rifax_days_he = number of days taking rifax since first HE event

he_data3 <- merge(he_data3, rifax_days, by= "id_number", all.x=TRUE)
he_data3 <- merge(he_data3, rifax_days_he, by= "id_number", all.x=TRUE)

#Inpatient and Outpatient

# inpatient_number = number of procedures listed that were inpatient after first cirrhosis event
# inpatient_number_he = number of procedures listed that were inpatient after first HE event

he_data3 <- merge(he_data3, inpatient_number, by= "id_number", all.x=TRUE)
he_data3 <- merge(he_data3, inpatient_number_he, by= "id_number", all.x=TRUE)


# outpatient_number = number of procedures listed that were either outpatient or part b after first cirrhosis event
# outpatient_number_he = number of procedures listed that were either outpatient or part b after first HE event

he_data3 <- merge(he_data3, outpatient_number, by= "id_number", all.x=TRUE)
he_data3 <- merge(he_data3, outpatient_number_he, by= "id_number", all.x=TRUE)

#Flagging subjects for attributes

#Inpatient and Outpatient, After Cirrhosis 

he_data3$inpatient_flag[he_data3$inpatient_num_procedures > he_data3$outpatient_num_procedures] <- 1
he_data3$inpatient_flag[he_data3$inpatient_num_procedures <= he_data3$outpatient_num_procedures] <- 0

#Inpatient and Outpatient, After HE

he_data3$inpatient_flag_he[he_data3$inpatient_num_procedures_he > he_data3$outpatient_num_procedures_he] <- 1
he_data3$inpatient_flag_he[he_data3$inpatient_num_procedures_he <= he_data3$outpatient_num_procedures_he] <- 0


#Benzo user, After First Event of Cirrhosis

he_data3$benzo_user[he_data3$benzo_days >= 90] <- 1
he_data3$benzo_user[he_data3$benzo_days < 90] <- 0

#Benzo user, After First Event of HE

he_data3$benzo_user_he[he_data3$benzo_days_he >= 90] <- 1
he_data3$benzo_user_he[he_data3$benzo_days_he < 90] <- 0


#Opiate user, After First Event of Cirrhosis

he_data3$opiate_user[he_data3$opiate_days >= 90] <- 1
he_data3$opiate_user[he_data3$opiate_days < 90] <- 0

#opiate user, After First Event of HE

he_data3$opiate_user_he[he_data3$opiate_days_he >= 90] <- 1
he_data3$opiate_user_he[he_data3$opiate_days_he < 90] <- 0



#ppi user, After First Event of Cirrhosis

he_data3$ppi_user[he_data3$ppi_days >= 90] <- 1
he_data3$ppi_user[he_data3$ppi_days < 90] <- 0

#ppi user, After First Event of HE

he_data3$ppi_user_he[he_data3$ppi_days_he >= 90] <- 1
he_data3$ppi_user_he[he_data3$ppi_days_he < 90] <- 0



#Rifaximin user, After First Event of Cirrhosis

he_data3$rifax_user[he_data3$rifax_days >= 90] <- 1
he_data3$rifax_user[he_data3$rifax_days < 90] <- 0

#Rifaximin user, After First Event of HE

he_data3$rifax_user_he[he_data3$rifax_days_he >= 90] <- 1
he_data3$rifax_user_he[he_data3$rifax_days_he < 90] <- 0


#Race Group 

he_data3$race_group[he_data3$race == 1] <- "White"
he_data3$race_group[he_data3$race == 2] <- "Black"
he_data3$race_group[he_data3$race != 1 & he_data3$race != 2] <- "Other"

#Portal Hypertension Covariates

he_data3$ascites_have[!is.na(he_data3$ascites_start) & he_data3$ascites_start <= he_data3$study_end] <- 1
he_data3$ascites_have[is.na(he_data3$ascites_start) | he_data3$ascites_start > he_data3$study_end] <- 0

he_data3$varices_have[!is.na(he_data3$varices_start) & he_data3$varices_start <= he_data3$varices_start] <- 1
he_data3$varices_have[is.na(he_data3$varices_start) | he_data3$varices_start > he_data3$varices_start] <- 0

he_data3$tips_have[!is.na(he_data3$tips_start) & he_data3$tips_start <= he_data3$study_end] <- 1
he_data3$tips_have[is.na(he_data3$tips_start) | he_data3$tips_start > he_data3$study_end] <- 0

he_data3$hecc_have[!is.na(he_data3$hecc_start) & he_data3$hecc_start <= he_data3$study_end] <- 1
he_data3$hecc_have[is.na(he_data3$hecc_start) | he_data3$hecc_start > he_data3$study_end] <- 0

he_data3$over_65[he_data3$age >65] <- 1
he_data3$over_65[he_data3$age <= 65] <- 0

he_data3$death_censor[he_data3$study_end == he_data3$true_death] <- 1
he_data3$death_censor[he_data3$study_end != he_data3$true_death | is.na(he_data3$true_death)] <- 0 
 
he_data3$personyear <- (he_data3$study_end - he_data3$study_start)/365.25 
he_data3$personyear_he <- (he_data3$study_end - he_data3$he_first)/365.25

he_data4 <- dplyr::distinct(he_data3, id_number, .keep_all=TRUE)

fwrite(he_data4,"Z:/data/devindata/final_dataset_beforereadmission")

readmisson_discharge_data <- fread("Z:/data/devindata/final_discharge_withreadmission")

he_data_final <- merge(he_data4,readmisson_discharge_data, by="id_number", all.x=TRUE)

fwrite(he_data_final,"Z:/data/devindata/final_dataset")


#### Tapper Dataset ###

tapper <- dplyr::select(he_data4, c(1:25,31:35,54:70))

tapper <- plyr::rename(tapper,c(ccif = "comorbidity"))

fwrite(tapper, "Z:/data/devindata/drtapper_dataset.csv")









#How deaths were retrieved

#Gets ID and Death Date from files. Then selects ID's that have death and makes a date in R form

# 
# #2008 Deaths
# 
# denom8 <- read_sas("Z:/lib/denom08.sas7bdat")
# 
# denom8b <- select(denom8,c("BENE_ID","BENE_DEATH_DT"))
# 
# denom8b <- filter(denom8b, BENE_DEATH_DT != "NaN")
# 
# denom8b$BENE_DEATH_DT <- as.Date(denom8b$BENE_DEATH_DT, '1960-01-01')
# 
# rm(denom8)
# 
# #2009 Deaths
# 
# denom9 <- read.sas7bdat("Z:/lib/denom09.sas7bdat")
# 
# denom9b <- select(denom9,c("BENE_ID","BENE_DEATH_DT"))
# 
# denom9b <-filter(denom9b, BENE_DEATH_DT != "NaN")
# 
# denom9b$BENE_DEATH_DT <- as.Date(denom9b$BENE_DEATH_DT, '1960-01-01')
# 
# rm(denom9)
# 
# #2010 Deaths
# 
# denom10 <- read.sas7bdat("Z:/lib/denom10.sas7bdat")
# 
# denom10b <- select(denom10,c("BENE_ID","BENE_DEATH_DT"))
# 
# denom10b <-filter(denom10b, BENE_DEATH_DT != "NaN")
# 
# denom10b$BENE_DEATH_DT <- as.Date(denom10b$BENE_DEATH_DT, '1960-01-01')
# 
# rm(denom10)
# 
# 
# #2011 Deaths
# 
# denom11 <- read.sas7bdat("Z:/lib/denom11.sas7bdat")
# 
# denom11b <- select(denom11,c("BENE_ID","BENE_DEATH_DT"))
# 
# denom11b <-filter(denom11b, BENE_DEATH_DT != "NaN")
# 
# denom11b$BENE_DEATH_DT <- as.Date(denom11b$BENE_DEATH_DT, '1960-01-01')
# 
# rm(denom11)
# 
# #2012 Deaths (different because I needed to rename variable to fit others)
# 
# denom12 <- read.sas7bdat("Z:/lib/denom12.sas7bdat")
# 
# denom12b <- select(denom12,c("BENE_ID","DEATH_DT"))
# 
# denom12b <- filter(denom12b, DEATH_DT != "NaN")
# 
# denom12b$BENE_DEATH_DT <- denom12b$DEATH_DT
# 
# denom12b$BENE_DEATH_DT <- as.Date(denom12b$BENE_DEATH_DT, '1960-01-01')
# 
# denom12b <- subset(denom12b, select = -DEATH_DT)
# 
# rm(denom12)
# 
# #2013 Deaths
# 
# denom13 <- read.sas7bdat("Z:/lib/denom13.sas7bdat")
# 
# denom13b <- select(denom13,c("BENE_ID","BENE_DEATH_DT"))
# 
# denom13b <- filter(denom13b, BENE_DEATH_DT != "NaN")
# 
# denom13b$BENE_DEATH_DT <- as.Date(denom13b$BENE_DEATH_DT, '1960-01-01')
# 
# rm(denom13)
# 
# #2014 Deaths
# 
# denom14 <- read.sas7bdat("Z:/lib/denom14.sas7bdat")
# 
# denom14b <- select(denom14,c("BENE_ID","BENE_DEATH_DT"))
# 
# denom14b <- filter(denom14b, BENE_DEATH_DT != "NaN")
# 
# denom14b$BENE_DEATH_DT <- as.Date(denom14b$BENE_DEATH_DT, '1960-01-01')
# 
# 
# 
# #Forms a dataset with all the deaths 
# 
# deaths <- rbind(denom8b,denom9b,denom10b,denom11b,denom12b,denom13b,denom14b)
# 
# #Writes into file for later keeping
# 
# fwrite(deaths,file ="Z:/data/devindata/realdeaths")
# 
# deaths <- fread("Z:/data/devindata/realdeaths")

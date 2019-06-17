#Gets the Discharge and Number of Readmission for each subject, taking into account transfers
# Based on having admission date greater or equal to first event of HE

library(tidyverse); library(lubridate); library(data.table); library(stringr)
library(lme4); library(survival); library(future); library(sas7bdat); library(ggplot2); library(survminer)

#Reads in the final dataset before adding in the readmission data

he_data <- fread("Z:/data/devindata/final_dataset_beforereadmission")

#Hospital Records

path= 'Z:/data/RData'
file12 = sprintf('Z:/data/RData/hosp_08to15.Rdata', path)
load(file12)

#Finds he_data for only HE patients

he_data_heonly <- subset(he_data, !is.na(he_data$he_first))

startandend_he <- dplyr::select(he_data_heonly, c("id","id_number","study_start","study_end","he_first"))

hospital_data_he <- merge(hosp, startandend_he, by="id")

hospital_data_he$admit_date <- ymd(hospital_data_he$admit_date)
hospital_data_he$disch_date <- ymd(hospital_data_he$disch_date)
hospital_data_he$he_first <- ymd(hospital_data_he$he_first)

#Subset Hospital records to only at or after HE event

hospital_he <- subset(hospital_data_he, hospital_data_he$admit_date >= hospital_data_he$he_first &  hospital_data_he$admit_date <= hospital_data_he$study_end)

hospital_he2 <- hospital_he[order(id_number,admit_date)]

#Find differences between admission date and last discharge

hospital_with_discharge_lapse <- hospital_he2%>% group_by(id) %>%  mutate(discharge_lapse = admit_date - lag(disch_date, default = 0))


#Gives a number for each record of hospital admission

hospital_with_discharge_lapse$seq <- ave(hospital_with_discharge_lapse$id, hospital_with_discharge_lapse$id, FUN= seq_along)

#### Finding which ones the first discharge is good ###

secondsequence <- subset(hospital_with_discharge_lapse, as.numeric(hospital_with_discharge_lapse$seq) == 2)

# Marks if second discharge is good, greater than 0 days of difference

secondsequence$firstdischargegood[secondsequence$discharge_lapse > 0] <- 1
secondsequence$firstdischargegood[secondsequence$discharge_lapse == 0] <- 0

secondsequence2 <- dplyr::select(secondsequence, c(11,17))

secondsequence2$id <- NULL

#Merges in the mark for if 2nd discharge is good

hospital_with_discharge_lapse2 <- merge(hospital_with_discharge_lapse, secondsequence2, by="id_number", all.x=TRUE)


#If no discharge ever, then marked as good

hospital_with_discharge_lapse2$firstdischargegood[is.na(hospital_with_discharge_lapse2$firstdischargegood)] <- 1


### Getting dates for patients that had transfers for first hospital ###


#Subset the id's without a good second discharge

notfirstdischarge <- subset(hospital_with_discharge_lapse2, hospital_with_discharge_lapse2$firstdischargegood == 0)

#Orders by id_number and sequence

notfirstdischarge <- notfirstdischarge[order(notfirstdischarge$id_number,as.numeric(notfirstdischarge$seq)),]

#Finds the last record

notfirstdischarge <- notfirstdischarge %>% group_by(id_number) %>% mutate(last_hosp = max(as.numeric(seq)))

#Finds the records that are either first discharge that is not transfer or is last record
getting_dates <- subset(notfirstdischarge, notfirstdischarge$seq != 1 & (notfirstdischarge$discharge_lapse != 0 | notfirstdischarge$seq == notfirstdischarge$last_hosp))

#Selects the first discharge that is either not transfer or last record

notfirstdischarge_dates <- distinct(getting_dates, id_number, .keep_all = TRUE)

notfirstdischarge_dates2 <- dplyr::select(notfirstdischarge_dates,c(1,7))

#Renames column discharge_date to not first date for later merge

#Have separate datasets for ones with good 2nd discharge and ones that didnt

notfirstdischarge_dates2 <- plyr::rename(notfirstdischarge_dates2,c("disch_date" = "notfirst_discharge_date"))

#Getting dates for first discharge


firstdischarge <- subset(hospital_with_discharge_lapse2, hospital_with_discharge_lapse2$firstdischargegood == TRUE)

firstdischarge_dates <- subset(firstdischarge, firstdischarge$seq == 1)

firstdischarge_dates2 <- dplyr::select(firstdischarge_dates,c(1,7))

firstdischarge_dates2 <- plyr::rename(firstdischarge_dates2,c("disch_date" = "first_discharge_date"))

#Merging in the new discharge dates

#Merges in hospital records with discharge dates, have mark for if first discharge good

hospital_with_discharge_lapse3 <- merge(hospital_with_discharge_lapse2, firstdischarge_dates2, by = "id_number", all.x=TRUE)
hospital_with_discharge_lapse3 <-  merge(hospital_with_discharge_lapse3, notfirstdischarge_dates2, by = "id_number", all.x=TRUE)

#Based on if first discharge is transfer, puts in true discharge date

hospital_with_discharge_lapse3$true_discharge_date[hospital_with_discharge_lapse3$firstdischargegood == 1] <- as.character(hospital_with_discharge_lapse3$first_discharge_date[hospital_with_discharge_lapse3$firstdischargegood == 1])

hospital_with_discharge_lapse3$true_discharge_date[hospital_with_discharge_lapse3$firstdischargegood == 0] <- as.character(hospital_with_discharge_lapse3$notfirst_discharge_date[hospital_with_discharge_lapse3$firstdischargegood == 0])

hospital_with_discharge_lapse3$true_discharge_date <- ymd(hospital_with_discharge_lapse3$true_discharge_date)

#Getting number of readmissions

hospital_with_discharge_lapse3 <- hospital_with_discharge_lapse3 %>% group_by(id_number) %>%  mutate(max_number = max(as.numeric(seq)))

#Finds number of readmissions after first true discharge

readmission_data <- subset(hospital_with_discharge_lapse3, hospital_with_discharge_lapse3$admit_date > hospital_with_discharge_lapse3$true_discharge_date & hospital_with_discharge_lapse3$discharge_lapse >0)

readmission_data$number_for_readmission <- ave(readmission_data$id_number, readmission_data$id_number, FUN= seq_along)

readmission_data2 <- readmission_data %>% group_by(id_number) %>%  summarise(number_of_readmissions_he = max(as.numeric(number_for_readmission)))

#Merges in the discharge dates with readmission information counts

final_discharge_dataset <- merge(hospital_with_discharge_lapse3, readmission_data2, by="id_number", all.x=TRUE)

#If no record, zero readmissions

final_discharge_dataset$number_of_readmissions_he[is.na(final_discharge_dataset$number_of_readmissions_he)] <- 0

#Keeps only the distinct IDs, if there is copy, first one kept

discharge_data <- distinct(final_discharge_dataset, id_number, .keep_all= TRUE)

discharge_data <- dplyr::select(discharge_data, c(1,20,22))


#Getting Admission Dates

firstadmit <- subset(hospital_with_discharge_lapse, as.numeric(hospital_with_discharge_lapse$seq) == 1)

firstadmit_dates <- dplyr::select(firstadmit, c(11,2))

firstadmit_dates<- plyr::rename(firstadmit_dates,c("admit_date" = "first_admit_date"))


firstadmit_dates$id <- NULL

discharge_data_final <- merge(discharge_data, firstadmit_dates)

fwrite(discharge_data_final,"Z:/data/devindata/final_discharge_withreadmission")

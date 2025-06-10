rm(list = ls())
gc()
library(RMySQL)
library(dplyr)
library(lubridate)
db <- dbConnect(MySQL(), default.file = "./.my.cnf", group = "yangfan")

setwd("S:/ECHO_IHI_CPRD/Extract #2/Example code/cohort_extract_tutorial/")

dbSendQuery(con = db, statement = "DROP TABLE IF EXISTS template_patient_info")

# .........save patient files...........#
stacked_patid <- dbSendQuery(db, "
select *
from yangfan.all_index_pat")


stacked_pat<-fetch(stacked_patid,-1)

# Recode the ethnicities directly in the existing column
stacked_pat$gen_ethnicity[stacked_pat$gen_ethnicity %in% c("Indian", "Bangladesi", "Pakistani", "Oth_Asian")] <- "Asian"
stacked_pat$gen_ethnicity[stacked_pat$gen_ethnicity %in% c("Bl_Afric", "Bl_Carib", "Bl_Other","Black")] <- "Black"
stacked_pat$gen_ethnicity[stacked_pat$gen_ethnicity %in% c("Other", "Unknown")] <- "Other/Unknown"

# View the updated counts
table(stacked_pat$gen_ethnicity)
sum(is.na(stacked_pat$gen_ethnicity)) #3050632
stacked_pat$eth_from_hes <- 1

gp_eth <- dbSendQuery(db, "
select *
from yangfan.lat_gp_eth")

pat_gp_eth<-fetch(gp_eth,-1)
#sum(is.na(pat_gp_eth$ethnic)) #0
pat_gp_eth$eth_from_hes <- 0

pat_gp_eth<-pat_gp_eth%>%mutate(ethnic=recode_factor(
  ethnic,"Other/Unknow"="Other/Unknown"))
#table(pat_gp_eth$ethnic)
#table(stacked_pat$gen_ethnicity)
pat_gp_eth$ethnic<-as.character(pat_gp_eth$ethnic)

combined_pat <- full_join(stacked_pat, pat_gp_eth, by = "e_patid")

# Fill missing values in gen_ethnicity with Eth_category from hes_pat
combined_pat <- combined_pat %>%
  mutate(gen_ethnicity = ifelse(is.na(ethnic),gen_ethnicity, ethnic)) %>%
  select(-ethnic)  

combined_pat <- combined_pat %>%
  mutate(hes_eth = eth_from_hes.y+eth_from_hes.x) %>%
  select(-eth_from_hes.y, -eth_from_hes.x)  # Remove the unnecessary Eth_category column

combined_pat<-combined_pat%>%mutate(hes_eth=replace(hes_eth,is.na(hes_eth),0))
combined_pat<-combined_pat%>%mutate(gen_ethnicity=ifelse(is.na(gen_ethnicity),"Missing",gen_ethnicity))
#table(combined_pat$gen_ethnicity)
#names(combined_pat)[colSums(is.na(combined_pat))>0]
combined_pat$deathdate<-as.Date(combined_pat$deathdate)
combined_pat$diagnosisdate<-as.Date(combined_pat$diagnosisdate)
combined_pat$index_date<-as.Date(combined_pat$index_date)

combined_pat<-combined_pat%>%
  mutate(futime=as.numeric(pmin(deathdate,diagnosisdate,index_date+years(1),na.rm=TRUE)-index_date)/365.25)

summary(combined_pat$futime)    

saveRDS(combined_pat, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_index_pat.rds")

#Divided stacked dataset into individual index datasets
combined_pat$index_date <- as.Date(combined_pat$index_date, format = "%Y-%m-%d")

combined_pat <- combined_pat[order(combined_pat$index_date), ]

unique_dates <- unique(combined_pat$index_date)

for (i in seq_along(unique_dates)) {
  date <- unique_dates[i]
  subset_data <- subset(combined_pat, index_date == date)%>%distinct()
  subset_data<-subset_data%>%group_by(e_patid)%>%filter(!(n()>1&crc_in_1_yr==0))%>%ungroup
  dup<-subset_data%>%group_by(e_patid)%>%filter(n()>1)
  print(nrow(dup))
  file_name <- sprintf("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_patient/pat_idx_%02d.rds", i)
  saveRDS(subset_data, file = file_name)
}

#Try
#pat10<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_patient/pat_idx_01.rds")
#names(pat01)[colSums(is.na(pat01))>0]
#summary(pat01$futime)
#table(pat01$gen_ethnicity)
#table(pat01$crc_in_1_yr)
#table(pat01$gender)
#crc_pat<-combined_pat%>%filter(crc_in_1_yr==1)%>%select(e_patid,gender,gen_ethnicity,imd2015_10)%>%distinct(e_patid)
#----------------------------------------#

#Extract each variable
BMI <- dbSendQuery(db, "
select *
from yangfan.bmi")

bmi<-fetch(BMI,-1)

saveRDS(bmi, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/bmi.rds")

#2 here because this is the updated code
Biomarker2 <- dbSendQuery(db, "
select *
from yangfan.biomarkers2")

biomarkers2<-fetch(Biomarker2,-1)

saveRDS(biomarkers2, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/biomarkers2.rds")

Symptom <- dbSendQuery(db, "
select *
from yangfan.all_pat_symptoms")

symptoms<-fetch(Symptom,-1)

saveRDS(symptoms, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/symptoms.rds")

Condition <- dbSendQuery(db, "
select *
from yangfan.all_pat_conditions")

conditions<-fetch(Condition,-1)

saveRDS(conditions, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/conditions.rds")

smoking<-dbSendQuery(db, "
select *
from yangfan.smoking_all")

Smoking<-fetch(smoking,-1)

saveRDS(Smoking, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/smoking.rds")

drinking<-dbSendQuery(db, "
select *
from yangfan.all_pat_drinking")

Drinking<-fetch(drinking,-1)

saveRDS(Drinking, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/drinking.rds")

colo_gp<-dbSendQuery(db, "
select *
from yangfan.colo_screen_GP")

colo_screen_GP<-fetch(colo_gp,-1)

saveRDS(colo_screen_GP, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/colo_screen_GP.rds")

colo_hes<-dbSendQuery(db, "
select *
from yangfan.colo_screen_hes")

colo_screen_hes<-fetch(colo_hes,-1)

saveRDS(colo_screen_hes, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/colo_screen_hes.rds")

Aspirin<-dbSendQuery(db, "
select *
from yangfan.aspirin_all")

aspirin<-fetch(Aspirin,-1)

saveRDS(aspirin, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/aspirin.rds")

NSAIDs<-dbSendQuery(db, "
select *
from yangfan.NSAIDs_all")

NSAIDs_data<-fetch(NSAIDs,-1)

saveRDS(NSAIDs_data, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/nsaids.rds")

#Distinct patient ID
distinct_patid<-dbSendQuery(db, "
select *
from yangfan.distinct_patid")

all_pat<-fetch(distinct_patid,-1)
saveRDS(all_pat, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/all_patid.rds")








pat<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_index_pat.rds")
lat<-pat%>%
  group_by(e_patid)%>%filter(index_date==max(index_date))%>%ungroup()
lat_white<-lat%>%filter(gen_ethnicity=="White")
lat_non_white<-lat%>%filter(gen_ethnicity!="White")
table(lat_non_white$crc_in_1_yr)
lat_non_white$gp_time<-as.numeric(as.Date(lat_non_white$gp_end)-as.Date(lat_non_white$gp_start))/365.25
summary(lat_non_white$gp_time)
lat_white$gp_time<-as.numeric(as.Date(lat_white$gp_end)-as.Date(lat_white$gp_start))/365.25
summary(lat_white$gp_time)
lat_hes_non_white<-lat_non_white%>%filter(hes_eth==1)
lat_hes_non_white$gp_time<-as.numeric(as.Date(lat_hes_non_white$gp_end)-as.Date(lat_hes_non_white$gp_start))/365.25
summary(lat_hes_non_white$gp_time)
table(lat_hes_non_white$crc_in_1_yr)

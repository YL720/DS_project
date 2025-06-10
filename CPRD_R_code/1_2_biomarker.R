#Biomarker Variables:
#Iron-deficincy, Inflammation (measured, abnormal)
rm(list = ls())
gc()

#Reload because of memory limitation
library(dplyr)
library(tidyr)
library(lubridate)
source("N:/Desktop/yl_cam/ACED_CODE/biomarker_manage_march2022.r")
source("N:/Desktop/yl_cam/ACED_CODE/biomarker_cleaning.r")
source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
source("N:/Desktop/yl_cam/ACED_CODE/colorectal_biomarker_covariates.r")

df_stack_pat<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_index_pat.rds")%>%
                      select(e_patid,yob,gender)%>%
                      distinct(e_patid,yob,gender)

index_dates<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_index_pat.rds")%>%
  select(index_date)%>%
  distinct(index_date)

biomarkers<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/biomarkers2.rds")

biomarkers<-biomarkers%>%select(e_patid,biomarker=Biomarker,eventdate,value=data2,data3)
biomarkers<-left_join(biomarkers,df_stack_pat,by='e_patid')
biomarkers$eventdate<-as.Date(biomarkers$eventdate)
biomarkers$event_age<- year(biomarkers$eventdate)-biomarkers$yob
biomarkers$sex<-biomarkers$gender
biomarkers$eid<-as.character(biomarkers$e_patid)

biomarkers<-biomarkers%>%select(-gender,-e_patid)
biomarkers<-biomarkers%>%filter((eventdate >= as.Date("2005-01-01")) & (eventdate <= as.Date("2017-01-01")))
biomarkers<-biomarker_basic_clean_CRC(biomarkers)
n_distinct(biomarkers$eid)
num_years_back<-2

get_df_gp_biom_vars <- function(j) {
  index_date<-index_dates[j,1]
  # Convert index_date to Date type if it's not already
  index_date <- as.Date(index_date)
  
  # Limit biomarkers events to dates within window ----
  biomarkers$window_min <- index_date - years(num_years_back) 
  biomarkers$window_max <- index_date
  df_gp_biom_window <-biomarkers%>%filter((eventdate >= window_min) & (eventdate <= window_max))
  
  # Identify number of measurements and abnormal results for each biomarker per eid
  df_gp_biom_meta <- biomarker_meta_CRC(df_gp_biom_window) # By eid, number of tests and number of abnormal results (both per test type)
  
  # iron deficiency
  df_gp_biom_meta <- iron_def_vars(df_gp_biom_meta)
  # inflammation
  df_gp_biom_meta <- inflam_vars(df_gp_biom_meta)
  # raised CA125
  df_gp_biom_meta <- CA125_raised_vars(df_gp_biom_meta)
  df_gp_biom_meta <- subset(df_gp_biom_meta, select = -sex)

  save(df_gp_biom_meta, file = file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/biomarkers/df_gp_biom_index_date_", j, ".Rdata")
                                         ) )
}

#Parallel computing
library(parallel)
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
  library(purrr)
  library(arrow)
  library(tidyr)
  source("N:/Desktop/yl_cam/ACED_CODE/biomarker_manage_march2022.r")
  source("N:/Desktop/yl_cam/ACED_CODE/biomarker_cleaning.r")
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
  source("N:/Desktop/yl_cam/ACED_CODE/colorectal_biomarker_covariates.r")
})

clusterExport(cl, c("index_dates","biomarkers","num_years_back"))

result <- parLapply(cl, 1:45, get_df_gp_biom_vars)


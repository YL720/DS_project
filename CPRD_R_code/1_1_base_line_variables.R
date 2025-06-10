#Baseline variable: 
#BMI, Drinking, Smoking
drinking <-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/drinking.rds")
smoking<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/smoking.rds")
bmi<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/bmi.rds")
index_dates<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_index_pat.rds")%>%
  select(index_date)%>%
  distinct(index_date)

process_data <- function(j) {
   index_date <- as.Date(index_dates[j, 1])
   message(paste("Started processing index:", j))
   #df_EHR is patient id 
   df_EHR <- readRDS(sprintf("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_patient/pat_idx_%02d.rds", j)) %>% select(e_patid)
   df_EHR$cutoff_max <- index_date
   #Any drinking recorded in look back period
   df_drinking <- merge(drinking, df_EHR, all.x = TRUE)%>% mutate(eid=e_patid)
   df_drinking$event_dt<-as.Date(df_drinking$eventdate)
   df_drinking <- df_drinking %>% filter(event_dt <= cutoff_max) %>% select(-e_patid)
   df_drinking_by_eid <- df_drinking %>% group_by(eid) %>% filter(row_number()==1) %>% 
                         mutate(drinking_ever=1) %>% select("eid",  contains(c("_ever"))) %>% ungroup 
   #n_distinct(df_drinking_by_eid$eid)
   #nrow(df_drinking_by_eid)
   #Smoking status in look back period
   df_smoking <- merge(smoking, df_EHR, all.x = TRUE)%>% mutate(eid=e_patid)
   df_smoking$eventdate<-as.Date(df_smoking$eventdate)
   df_smoking <- df_smoking %>% filter(eventdate <= cutoff_max) %>% select(-e_patid)
   df_smoking <- df_smoking %>% group_by(eid) %>% slice_max(eventdate) %>% ungroup()
   #Remove multiple different
   conf_eids_smoking <- df_smoking %>%
     group_by(eid) %>%
     filter(n_distinct(smokingcat) > 1) %>%
     pull(eid)
   
   df_smoking_cleaned <- df_smoking %>%
     filter(!eid %in% conf_eids_smoking)
   
   df_smoking_by_eid <- df_smoking_cleaned %>% ungroup()
   n_distinct(df_smoking_by_eid$eid)
   nrow(df_smoking_by_eid)
   
   df_bmi <- merge(bmi, df_EHR, all.x = TRUE)%>% mutate(eid=e_patid) 
   df_bmi$weight_date<-as.Date(df_bmi$weight_date)
   df_bmi <- df_bmi %>% filter(weight_date <= cutoff_max) %>% select(-e_patid)
   df_bmi_by_eid <- df_bmi %>% group_by(eid) %>%slice_max(weight_date) %>% 
     select(-cutoff_max,-weight_date,-height,-weight)%>% 
     slice_max(BMI)%>%
     ungroup()
   df_bmi_by_eid<-df_bmi_by_eid%>%distinct()
   n_distinct(df_bmi_by_eid$eid)
   nrow(df_bmi_by_eid)
   
   df_baseline_all<-df_drinking_by_eid %>% full_join(df_smoking_by_eid, by = "eid")%>% full_join(df_bmi_by_eid, by = "eid")
   df_baseline_all<-df_baseline_all %>% mutate(drinking_ever=ifelse(is.na(drinking_ever),0,drinking_ever))
   n_distinct(df_baseline_all$eid)
   nrow(df_baseline_all)
   
   save(df_baseline_all, file = file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/baseline_variable/df_basevars_index_date", j, ".Rdata")))
   
   return(NULL)
}

#parallel computing
library(parallel)
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
})

clusterExport(cl, c("index_dates", "drinking", "bmi", "smoking"))

result <- parLapply(cl, 1:45, process_data)

stopCluster(cl)

   
   
         
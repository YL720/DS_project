#long term conditions and medicine recorded

df_clinical_events <-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/conditions.rds")
df_clinical_events$event_dt <- as.Date(df_clinical_events$eventdate)
df_clinical_events <- df_clinical_events %>% select(-eventdate)
ever_events = c("IBD", "DIB_T1D", "DIB_T2D", "GALL") # long term conditions

df_scripts_NSAIDs <- readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/nsaids.rds")%>% select(e_patid,eventdate)%>%
                     distinct(e_patid,eventdate) %>% mutate(script_type = "NSAIDs",)
df_scripts_aspirin <- readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/aspirin.rds") %>% select(e_patid,eventdate)%>%
                      distinct(e_patid,eventdate) %>%mutate(script_type = "ASPIRIN")
df_script_events <- rbind(df_scripts_NSAIDs, df_scripts_aspirin)
df_script_events$issue_date <- as.Date(df_script_events$eventdate)
df_script_events <- df_script_events %>% select(-eventdate)
regular_scripts <- c("NSAIDs", "ASPIRIN")

process_data <- function(j) {
  index_date <- as.Date(index_dates[j, 1])
  message(paste("Started processing index:", j))
  df_EHR <- readRDS(sprintf("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_patient/pat_idx_%02d.rds", j)) %>% select(e_patid)
  df_EHR$cutoff_max <- index_date
  df_EHR$two_yr_lb <- index_date - years(2)
  df_events_plus <- merge(df_clinical_events, df_EHR, all.x = TRUE)
  df_events_plus <- df_events_plus %>% rename(eid = e_patid, event_type = conditions)
  
  df_events_plus <- df_events_plus %>% filter(event_dt <= cutoff_max)
  df_events_by_eid <- df_events_plus %>% group_by(eid)
  
  for (i in ever_events) {
    df_events_by_eid <- event_ever(df_events_by_eid, i)
  }
  
  #get df with one row per eid
  df_events_by_eid_ungroup <- df_events_by_eid %>% 
    filter(row_number()==1) %>% 
    select("eid",  contains(c("_ever"))) %>% 
    ungroup    
  
  df_scripts_plus <- merge(df_script_events, df_EHR, all.x = TRUE)
  df_scripts_plus <- df_scripts_plus %>% rename(eid = e_patid)
  df_scripts_plus <- df_scripts_plus %>% filter(issue_date <= cutoff_max)
  
  df_scripts_by_eid <- df_scripts_plus %>%  group_by(eid) 
  
  #find regular prescriptions
  for (i in regular_scripts) {
    df_scripts_by_eid <- script_reg_ever(df_scripts_by_eid, i, 4, 1)
  }
  
  #get df by eid
  df_scripts_by_eid_ungroup <- df_scripts_by_eid %>% 
    filter(row_number()==1) %>% 
    select("eid",  contains(c("reg_ever"))) %>% 
    ungroup 
  
  #merge in with whole EHR pop for lma
  df_scripts_by_eid_ungroup[is.na(df_scripts_by_eid_ungroup)] <- 0
  df_scripts_by_eid_ungroup <- df_scripts_by_eid_ungroup %>% select(eid,  ends_with("reg_ever"))
  
  
  #COMBINE CLINICAL AND PRESCRIPTION INFO
  df_gp_modifier_all <- full_join(df_events_by_eid_ungroup, df_scripts_by_eid_ungroup, by = "eid")
  
  save(df_gp_modifier_all, file = file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/modifier_events/df_gp_marker_events_index_date", j, ".Rdata")))
 
  return(NULL)
}

cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
})

clusterExport(cl, c("index_dates", "df_clinical_events", "df_script_events", "ever_events", "regular_scripts","event_ever","script_reg_ever"))

result <- parLapply(cl, 1:45, process_data)

stopCluster(cl)

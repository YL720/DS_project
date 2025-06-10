#All alarm and non-alarm symptoms and conditions

library(parallel)
library(dplyr)
library(lubridate)

source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")

any_in_lb_events <- c("ABDO_LUMP", "RECTAL_MASS", "CHANGE_BH", "RECTAL_BLEED") # alarm symptoms
freq_in_lb_events <- c("ABDO_BLOAT", "ABDO_PAIN", "PELVIC_PAIN", "STOM_DIS") # non-alarm symptoms
new_onset_events <- c("CONS", "DIARR", "WEIGHT_LOSS", "JAUNDICE", "FATIGUE", "DIV", "IBS", "HAEMR") # new onset symptoms or possible mis-diagnoses

df_clinical_events <- readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/symptoms.rds")
df_clinical_events$event_dt <- as.Date(df_clinical_events$eventdate)
df_clinical_events <- df_clinical_events %>% select(-eventdate)

process_data <- function(j) {
  index_date <- as.Date(index_dates[j, 1])
  message(paste("Started processing index:", j))
  #Patient id of index-dataset
  df_EHR <- readRDS(sprintf("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_patient/pat_idx_%02d.rds", j)) %>% select(e_patid)
  df_EHR$cutoff_max <- index_date
  df_EHR$two_yr_lb <- index_date - years(2)
  #New onset cutoff
  df_EHR$five_yr_lb <- index_date - years(5)
  df_events_plus <- merge(df_clinical_events, df_EHR, all.x = TRUE)
  df_events_plus <- df_events_plus %>% rename(eid = e_patid, event_type = symptom)
  df_events_plus <- df_events_plus %>% filter(event_dt <= cutoff_max)
  df_events_by_eid <- df_events_plus %>% group_by(eid)
  
  # alarm symptoms (any in lb)
  for (i in any_in_lb_events) {
    df_events_by_eid <- event_in_lb(df_events_by_eid, i)
  }
  # non-alarm symptoms (freq in lb)
  for (i in freq_in_lb_events) {
    df_events_by_eid <- event_freq_in_lb(df_events_by_eid, i)
  }
  # new onset symptoms (or misdiagnoses)
  for (i in new_onset_events) {
    df_events_by_eid <- event_new_onset(df_events_by_eid, i)
  }
  
  # get df with one row per eid
  df_events_by_eid_ungroup <- df_events_by_eid %>%
    filter(row_number() == 1) %>%
    select("eid", contains(c("_ever", "_in_lb", "new_onset"))) %>%
    ungroup()
  
  save(df_events_by_eid_ungroup, file = file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/marker_events/df_gp_marker_events_index_date", j, ".Rdata")))
  
  message(paste("Finished processing index:", j))
  
  return(NULL)
}

cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
})

clusterExport(cl, c("index_dates", "df_clinical_events", "any_in_lb_events", "freq_in_lb_events", "new_onset_events", "event_in_lb", "event_freq_in_lb", "event_new_onset"))

result <- parLapply(cl, 1:45, process_data)

stopCluster(cl)




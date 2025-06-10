#Colo_screen recorded from both HES and GP data，
#so write in a separate file
source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")

df_stack_pat<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_index_pat.rds")%>%
  select(e_patid,yob,gender,index_date)%>%
  distinct(e_patid,yob,gender,index_date)

index_dates<-df_stack_pat%>%distinct(index_date)

colo_screen_gp<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/colo_screen_GP.rds")
colo_screen_gp<-colo_screen_gp %>% mutate(hes=0) %>% select(e_patid,hes,eventdate)

colo_screen_hes<-readRDS("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/colo_screen_hes.rds")
colo_screen_hes<-colo_screen_hes %>% mutate(eventdate=evdate,hes=1) %>% select(e_patid,hes,eventdate)

colo_screen<-bind_rows(colo_screen_hes,colo_screen_gp)

process_data <- function(j) {
  index_date <- as.Date(index_dates[j, 1])
  message(paste("Started processing index:", j))
  df_EHR <- readRDS(sprintf("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_patient/pat_idx_%02d.rds", j)) %>% select(e_patid)
  df_EHR$cutoff_max <- index_date
  df_colo_screen <- merge(colo_screen, df_EHR, all.x = TRUE)%>% mutate(eid=e_patid)
  df_colo_screen$eventdate<-as.Date(df_colo_screen$eventdate)
  df_colo_screen <- df_colo_screen %>% filter(eventdate <= cutoff_max) %>% select(-e_patid,hes)%>%mutate(colo_screen=1)
  df_colo_screen_by_eid <- df_colo_screen %>% group_by(eid) %>%slice_max(eventdate) %>% select(-cutoff_max,-eventdate)%>% ungroup 

  save(df_colo_screen_by_eid, file = file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/other_events/df_other_event", j, ".Rdata")))
  
  return(NULL)
}


cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
})

clusterExport(cl, c("index_dates", "colo_screen"))

result <- parLapply(cl, 1:45, process_data)

stopCluster(cl)














rm(list=ls())
gc()
library(mice)
library(dplyr)
library(arrow)
library(parallel)
library(data.table)

#load data
stacked_data<-read_parquet("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_dataset_back2_fwd1.parquet")%>% 
  filter(!is.na(core_imd))
#check missing
any(is.na(stacked_data)) %>% cat("stacked_data any NAs......", .,"\n")

pat_num<-as.data.frame(table(stacked_data$index))
pat_num$Var1<-index_dates$index_date

write.csv(pat_num,"S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/patient_num.csv")
data_list <- split(stacked_data, stacked_data$index)

si_nelsonaalen <- function(data) {
  hazard <- survival::basehaz(survival::coxph(survival::Surv(data$futime, data$status) ~ 1))
  idx <- match(signif(data$futime, digits=10), signif(hazard[, "time"], digits=10))
  nelaa <- hazard[idx, "hazard"]
  cat("any NAs in nelaa......", any(is.na(nelaa)), "\n")
  return(nelaa)
}

stacked_data <- stacked_data %>% 
  dplyr::mutate(nelaa = si_nelsonaalen(stacked_data)) %>% 
  dplyr::mutate(status = as.integer(status), eid = as.factor(eid))

is.na(stacked_data) %>% any() %>% cat("pre-MICE data any NAs......", .,"\n")

mat_pred <- mice::quickpred(stacked_data, mincor = 0.05, minpuc = 0, exclude = c("eid", "futime"), method = "spearman")

examine_matpred <- mat_pred %>% as.data.frame()
rownames(examine_matpred) <- names(examine_matpred)
cat("Starting imputation......\n")
imputed_data <- mice::mice(stacked_data, m = 1, maxit = 10, pred = mat_pred, seed = 137)

completed_data <- mice::complete(imputed_data, action = 1L)
is.na(completed_data) %>% any() %>% cat("post-MICE data any NAs......", .,"\n")
write_parquet(completed_data, file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/complete_stacked_data.parquet")))

#imputed index by index

process_data <- function(data) {
  index <- unique(data$index)
  if(length(index)!=1){
    stop("mistake")
  }
  data <- data %>% select(-index)
  
  data <- data %>% 
    dplyr::mutate(nelaa = si_nelsonaalen(data)) %>% 
    dplyr::mutate(status = as.integer(status), eid = as.factor(eid))
  
  is.na(data) %>% any() %>% cat("pre-MICE data any NAs......", .,"\n")
  
  mat_pred <- mice::quickpred(data, mincor = 0.05, minpuc = 0, exclude = c("eid", "futime"), method = "spearman")
  
  examine_matpred <- mat_pred %>% as.data.frame()
  rownames(examine_matpred) <- names(examine_matpred)
  cat("Starting imputation......\n")
  imputed_data <- mice::mice(data, m = 1, maxit = 10, pred = mat_pred, seed = 137)
  
  completed_data <- mice::complete(imputed_data, action = 1L)
  is.na(completed_data) %>% any() %>% cat("post-MICE data any NAs......", .,"\n")
  completed_data$index <- index
  write_parquet(completed_data, file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/complete/complete_data_", index, ".parquet")))
  return(index)
}

cl <- makeCluster(detectCores() - 1)

clusterExport(cl, c("si_nelsonaalen", "mice", "data.table", "process_data"))
clusterEvalQ(cl, library(dplyr))
clusterEvalQ(cl, library(survival))
clusterEvalQ(cl, library(arrow))

parLapply(cl, data_list, process_data)
stopCluster(cl)

#combine all imputed datasets
all_imputed_files<-list.files(path="S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/complete/", full.names = TRUE)

stacked_data <- all_imputed_files %>%
  lapply(read_parquet) %>% 
  bind_rows() 

table(stacked_data$index)
n_distinct(stacked_data$eid)
colnames(stacked_data)[colSums(is.na(stacked_data))>0]
write_parquet(stacked_data, "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_complete_dataset_back2_fwd1.parquet")


#eth_minor<-stacked_data%>%filter(core_eth!="White")%>%select(eid)%>%distinct()

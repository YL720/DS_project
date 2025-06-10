library(survival)
library(dplyr)
library(parallel)
library(arrow)
library(fst)
library(caret)

# Load the complete dataset
complete_dataset <- read_parquet("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_complete_dataset_back2_fwd1.parquet")

# Split dataset by 'index' and store model coefficients
all_indexes <- unique(complete_dataset$index)
results <- list()
c_index_results <- list()

for (idx in all_indexes) {
  # Filter dataset by current index
  task <- complete_dataset %>% filter(index == idx)
  
  # Preprocess data
  task <- task %>%
    rename(core_drinking_status = core_drinking_ever,
           core_smoking_status = core_smokingcat,
           core_sex_reported = core_sex,
           core_ethn_selfrep = core_eth,
           core_imd_fifth = core_imd,
           symp_all_CONS_new_onset = symp_CONS_new_onset,
           symp_all_DIARR_new_onset = symp_DIARR_new_onset,
           symp_all_HAEMR_new_onset = symp_HAEMR_new_onset,
           medhist_colonoscopy_ALL_in_10_yr_lb = medhist_colo_screen) %>%
    select(-nelaa, -core_yob)
  
  task$core_eligiable_screen <- NULL
  
  task <- task %>%
    mutate(core_smoking_status = recode(core_smoking_status,
                                        "Current smoker" = "current",
                                        "Ex smoker" = "previous",
                                        "Non-smoker" = "never"),
           core_imd_fifth = recode(core_imd_fifth,
                                   '1' = '(0,8.25]',
                                   '2' = '(8.25,13.6]',
                                   '3' = '(13.6,21.2]',
                                   '4' = '(21.2,34]',
                                   '5' = '(34,100]'),
           core_sex_reported = recode(core_sex_reported, '2' = '0'),
           core_sex_reported = relevel(core_sex_reported, ref = '0'),
           core_smoking_status = relevel(core_smoking_status, ref = 'never'),
           core_ethn_selfrep = relevel(core_ethn_selfrep, ref = 'Other/Unknown'),
           core_imd_fifth = relevel(core_imd_fifth, ref = '(0,8.25]'))
  
  task$core_age <- scale(task$core_age)
  task$core_age2 <- scale(task$core_age2)
  task$core_bmi <- scale(task$core_bmi)
  
  # Define full model formula
  feature_names <- colnames(task)
  feature_names <- feature_names[!feature_names %in% c("eid", "futime", "status", "index")]
  
  full_fml <- paste0("Surv(time = futime, event = status) ~ ", paste(c(feature_names), collapse = " + "))
  full_fit <- coxph(as.formula(full_fml), data = task)
  
  # K-fold cross-validation
  #set.seed(123)
  #folds <- createFolds(task$status, k = 10, list = TRUE, returnTrain = FALSE)
  #c_indexes <- c()
  
  #for (i in 1:10) {
  #  test_indices <- folds[[i]]
  #  train_data <- task[-test_indices, ]
  #  test_data <- task[test_indices, ]
    
  #  model <- coxph(as.formula(full_fml), data = train_data, x = TRUE)
  #  test_data$predicted <- predict(model, newdata = test_data, type = "risk")
    
  #  concordance_result <- survConcordance(Surv(test_data$futime, test_data$status) ~ test_data$predicted)
  #  c_index <- concordance_result$concordance
  #  c_indexes <- c(c_indexes, c_index)
 #}
  
  # Store mean c-index for this index
  #mean_c_index <- mean(c_indexes)
  #c_index_results[[as.character(idx)]] <- mean_c_index
  
  # Store model coefficients
  coeffs <- summary(full_fit)$coefficients
  coeffs_df <- as.data.frame(coeffs)
  coeffs_df$index <- idx
  results[[as.character(idx)]] <- coeffs_df
}

# Combine all coefficients into one dataframe and save as CSV
all_coeffs <- do.call(rbind, results)
write.csv(all_coeffs, "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_ds_model_coefficients.csv", row.names = FALSE)

# Save c-index results as CSV
c_index_df <- data.frame(index = names(c_index_results), c_index = unlist(c_index_results))
write.csv(c_index_df, "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_ds_c_index_results.csv", row.names = FALSE)

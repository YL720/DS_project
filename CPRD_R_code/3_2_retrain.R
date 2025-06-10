library(survival)
library(dplyr)
library(parallel)
library(arrow)
library(ggplot2)
library(fst)
# Load and preprocess the complete dataset
complete_dataset <- read_parquet("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_complete_dataset_back2_fwd1.parquet")

complete_dataset <- complete_dataset %>%
  filter((index - 1) %% 4 == 0) %>%
  rename(core_drinking_status = core_drinking_ever,
         core_smoking_status = core_smokingcat,
         core_sex_reported = core_sex,
         core_ethn_selfrep = core_eth,
         core_imd_fifth = core_imd,
         symp_all_CONS_new_onset = symp_CONS_new_onset,
         symp_all_DIARR_new_onset = symp_DIARR_new_onset,
         symp_all_HAEMR_new_onset = symp_HAEMR_new_onset,
         medhist_colonoscopy_ALL_in_10_yr_lb = medhist_colo_screen) %>%
  select(-nelaa, -index, -core_yob)

complete_dataset$core_eligiable_screen <- NULL

complete_dataset <- complete_dataset %>%
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

complete_dataset$core_age<-scale(complete_dataset$core_age)
complete_dataset$core_age2<-scale(complete_dataset$core_age2)
complete_dataset$core_bmi<-scale(complete_dataset$core_bmi)

write_fst(complete_dataset,"S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/superlandmark_dataset.fst")
#Full model
task <- complete_dataset

feature_names <- colnames(task)
feature_names <- feature_names[!feature_names %in% c("eid", "futime", "status")]

full_fml <- paste0("Surv(time = futime, event = status) ~ ", paste(c(feature_names), collapse = " + "))
full_fit <- coxph(as.formula(full_fml), data = task)

min_cols <- feature_names[grepl("^core_age|^core_sex", feature_names)]
min_fml <- paste0("Surv(time = futime, event = status) ~ ", ifelse(length(min_cols) == 0, 1, paste(min_cols, collapse = " + ")))
min_fit <- coxph(as.formula(min_fml), data = task)

fit_stepwise_bidirectional <- step(
  min_fit,
  scope = list(lower = min_fit, upper = full_fit),
  data = task, steps = 500, direction = "both"
)

covars_selected <- attributes(fit_stepwise_bidirectional$terms)$term.labels
cat("covars_selected......\n")
print(covars_selected)

covars_kept <- unique(covars_selected)
cat("covars_kept (inc forced)......\n")
print(covars_kept)

red_fml <- paste0("Surv(time = futime, event = status) ~ ", paste(c(covars_kept[!covars_kept %in% c("eid")], "cluster(eid)"), collapse = " + "))
if (length(covars_kept) == 0) {
  red_fml <- "Surv(time = futime, event = status) ~ 1"
}

cat("red_fml......\n")
print(red_fml)

mdl <- coxph(as.formula(red_fml), data = task, x = TRUE)
saveRDS(mdl, file = "S:/ECHO_IHI_CPRD/Data/Yangfan/bisw_cox_model.rds")
library(survminer)
library(caret)
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)

# K-fold
c_indexes <- c()
folds <- createFolds(task$status, k = 10, list = TRUE, returnTrain = FALSE)

for (i in 1:10) {
  i=1
  test_indices <- folds[[i]]
  train_data <- task[-test_indices, ]
  test_data <- task[test_indices, ]
  
  model <- coxph(as.formula(red_fml), data = train_data, x = TRUE)

  test_data$predicted <- predict(model, newdata = test_data, type = "risk")

  concordance_result <- survConcordance(Surv(test_data$futime,test_data$status)~test_data$predicted)
  c_index<-concordance_result$concordance
  c_indexes <- c(c_indexes, c_index)
}

print(c_indexes)
mean_c_index <- mean(c_indexes)
print(mean_c_index)

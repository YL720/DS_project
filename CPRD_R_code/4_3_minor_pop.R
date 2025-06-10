library(survival)
library(dplyr)
library(parallel)
library(arrow)
library(ggplot2)

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
         medhist_colonoscopy_ALL_in_10_yr_lb = medhist_colo_screen,
         medhist_bcscreen_eligible = core_eligiable_screen) %>%
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
         core_smoking_status = relevel(core_smoking_status, ref = 'never'),
         core_ethn_selfrep = relevel(core_ethn_selfrep, ref = 'Other/Unknown'),
         core_imd_fifth = relevel(core_imd_fifth, ref = '(0,8.25]'))
output_path<-"S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/minor_race"
model_path<-"N:/Desktop/yl_cam/UKB_model_bisw.rds"
process_and_calibrate <- function(complete_dataset, model_path, output_path, plot_name) {
  # Split the dataset into three parts
  total_rows <- nrow(complete_dataset)
  third_rows <- ceiling(total_rows / 3)
  df_part1 <- complete_dataset[1:third_rows, ]
  df_part2 <- complete_dataset[(third_rows + 1):(2 * third_rows), ]
  df_part3 <- complete_dataset[(2 * third_rows + 1):total_rows, ]
  
  process_data <- function(data) {
    # Select applied data
    applied_data <- data %>% select(-futime, -status)
    
    # Standardize continuous variables
    continuous_vars <- sapply(applied_data, is.numeric)
    applied_data[continuous_vars] <- scale(applied_data[continuous_vars])
    
    # Load the model
    model <- readRDS(model_path)
    
    # Fit the survival model
    fit <- survfit(formula = model, newdata = applied_data, se.fit = FALSE)
    time <- fit$time
    surv <- fit$surv
    rm(fit)
    
    # Predict linear predictors
    lp <- predict(model, type = "lp", newdata = applied_data)
    rm(model)
    
    list(time = time, surv = surv, lp = lp)
  }
  
  # Process each part separately
  result_part1 <- process_data(df_part1)
  rm(df_part1)
  gc()
  
  result_part2 <- process_data(df_part2)
  rm(df_part2)
  gc()
  
  result_part3 <- process_data(df_part3)
  rm(df_part3)
  gc()
  
  # Combine the results
  time <- c(result_part1$time, result_part2$time, result_part3$time)
  surv <- c(result_part1$surv[1464,], result_part2$surv[1464,], result_part3$surv[1464,])
  lp <- c(result_part1$lp, result_part2$lp, result_part3$lp)
  rm(result_part1, result_part2, result_part3)
  gc()
  
  # Create survival object
  surv_obj <- with(complete_dataset, Surv(futime, status))
  
  # Compute C-index
  concordance <- survConcordance(surv_obj ~ lp)
  predicted_probs <- surv
  rm(surv)
  data <- data.frame(status = complete_dataset$status, risk = 1 - predicted_probs)
  
  data <- data %>%
    mutate(decile = ntile(risk, 10))
  
  observed_probabilities <- data %>%
    group_by(decile) %>%
    summarize(observed_probability = mean(status))
  
  mean_predicted_risks <- data %>%
    group_by(decile) %>%
    summarize(mean_predicted_risk = mean(risk))
  
  calibration_data <- left_join(mean_predicted_risks, observed_probabilities, by = "decile")
  calibration_data <- calibration_data %>%
    mutate(mean_predicted_risk = mean_predicted_risk * 100,
           observed_probability = observed_probability * 100)
  calibration_model <- lm(observed_probability ~ mean_predicted_risk, data = calibration_data)
  
  calibration_intercept <- coef(calibration_model)[1]
  calibration_slope <- coef(calibration_model)[2]
  
  conf_intervals <- confint(calibration_model)
  
  intercept_confint <- conf_intervals[1, ]
  slope_confint <- conf_intervals[2, ]
  
  results <- list(
    concordance = concordance$concordance,
    calibration_intercept = calibration_intercept,
    intercept_confint = intercept_confint,
    calibration_slope = calibration_slope,
    slope_confint = slope_confint
  )
  
  calibration_plot <- ggplot(calibration_data, aes(x = mean_predicted_risk, y = observed_probability)) +
    geom_point(color = "black") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(x = "Predicted Risk (%)", y = "Observed Risk (%)", title = "Calibration (at 1 year) for the study population") +
    theme_minimal() +
    xlim(0, 1) +
    ylim(0, 1)
  
  ggsave(file.path(output_path, plot_name), plot = calibration_plot, width = 8, height = 6)
  
  return(results)
}
subset0 <- complete_dataset 
plot_name0 <- "calibration_plot_all.png"
result0 <- process_and_calibrate(subset0, model_path, output_path, plot_name0)

# Define first subset
subset1 <- complete_dataset %>% filter(core_sex_reported == "0")
plot_name1 <- "calibration_plot_sex_0.png"
result1 <- process_and_calibrate(subset1, model_path, output_path, plot_name1)

# Release memory
rm(subset1)
gc()

# Define second subset
subset2 <- complete_dataset %>% filter(core_sex_reported == "1")
plot_name2 <- "calibration_plot_sex_1.png"
result2 <- process_and_calibrate(subset2, model_path, output_path, plot_name2)

# Release memory
rm(subset2)
gc()

# Define third subset
subset3 <- complete_dataset %>% filter(core_ethn_selfrep == "White")
plot_name3 <- "calibration_plot_ethnicity_white.png"
result3 <- process_and_calibrate(subset3, model_path, output_path, plot_name3)

# Release memory
rm(subset3)
gc()

# Define fourth subset
subset4 <- complete_dataset %>% filter(core_ethn_selfrep != "White")
plot_name4 <- "calibration_plot_ethnicity_non_white.png"
result4 <- process_and_calibrate(subset4, model_path, output_path, plot_name4)

# Release memory
rm(subset4)
gc()

# Define fifth subset
subset5 <- complete_dataset %>% filter(core_imd_fifth == "(0,8.25]")
plot_name5 <- "calibration_plot_imd_0_8.25.png"
result5 <- process_and_calibrate(subset5, model_path, output_path, plot_name5)

# Release memory
rm(subset5)
gc()

# Define sixth subset
subset6 <- complete_dataset %>% filter(core_imd_fifth == "(8.25,13.6]")
plot_name6 <- "calibration_plot_imd_8.25_13.6.png"
result6 <- process_and_calibrate(subset6, model_path, output_path, plot_name6)

# Release memory
rm(subset6)
gc()

# Define seventh subset
subset7 <- complete_dataset %>% filter(core_imd_fifth == "(13.6,21.2]")
plot_name7 <- "calibration_plot_imd_13.6_21.2.png"
result7 <- process_and_calibrate(subset7, model_path, output_path, plot_name7)

# Release memory
rm(subset7)
gc()

# Define eighth subset
subset8 <- complete_dataset %>% filter(core_imd_fifth == "(21.2,34]")
plot_name8 <- "calibration_plot_imd_21_34.png"
result8 <- process_and_calibrate(subset8, model_path, output_path, plot_name8)

# Release memory
rm(subset8)
gc()

# Define ninth subset
subset9 <- complete_dataset %>% filter(core_imd_fifth == "(34,100]")
plot_name9 <- "calibration_plot_imd_34_100.png"
result9 <- process_and_calibrate(subset9, model_path, output_path, plot_name9)

# Release memory
rm(subset9)
gc()

results <- list(result1 = result1,result2 = result2, result3 = result3, result4 = result4, result5 = result5, result6 = result6, result7 = result7, result8 = result8, result9 = result9)

result_to_df <- function(result, subset_name) {
  data.frame(
    Subset = subset_name,
    Concordance = result[["concordance"]],
    Calibration_Intercept = result[["calibration_intercept"]],
    Intercept_CI_Lower = result[["intercept_confint"]][1],
    Intercept_CI_Upper = result[["intercept_confint"]][2],
    Calibration_Slope = result[["calibration_slope"]],
    Slope_CI_Lower = result[["slope_confint"]][1],
    Slope_CI_Upper = result[["slope_confint"]][2]
  )
}

# Convert each result to data frame
df1 <- result_to_df(result1, "sex_0")
df2 <- result_to_df(result2, "sex_1")
df3 <- result_to_df(result3, "ethnicity_white")
df4 <- result_to_df(result4, "ethnicity_non_white")
df5 <- result_to_df(result5, "imd_0_8.25")
df6 <- result_to_df(result6, "imd_8.25_13.6")
df7 <- result_to_df(result7, "imd_13.6_21.2")
df8 <- result_to_df(result8, "imd_21_34")
df9 <- result_to_df(result9, "imd_34_100")

# Combine all data frames into one
all_results_df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9)

# Define output path
output_csv_path <- file.path(output_path, "calibration_results.csv")

# Save the combined data frame to CSV
write.csv(all_results_df, file = output_csv_path, row.names = FALSE)

age_groups <- list(
  "40-50" = complete_dataset %>% filter(core_age >= 40 & core_age < 50),
  "50-60" = complete_dataset %>% filter(core_age >= 50 & core_age < 60),
  ">60" = complete_dataset %>% filter(core_age >= 60)
)

results <- list()
for (group_name in names(age_groups)) {
  subset_data <- age_groups[[group_name]]
  plot_name <- paste0("calibration_plot_age_", gsub(">", "gt", group_name), ".png")
  result <- process_and_calibrate(subset_data, model_path, output_path, plot_name)
  results[[group_name]] <- result
  rm(subset_data)
  gc()
}

result_to_df <- function(result, subset_name) {
  data.frame(
    Subset = subset_name,
    Concordance = result[["concordance"]],
    Calibration_Intercept = result[["calibration_intercept"]],
    Intercept_CI_Lower = result[["intercept_confint"]][1],
    Intercept_CI_Upper = result[["intercept_confint"]][2],
    Calibration_Slope = result[["calibration_slope"]],
    Slope_CI_Lower = result[["slope_confint"]][1],
    Slope_CI_Upper = result[["slope_confint"]][2]
  )
}

all_results_df <- do.call(rbind, lapply(names(results), function(name) result_to_df(results[[name]], name)))
output_csv_path <- file.path(output_path, "calibration_results.csv")
write.csv(all_results_df, file = output_csv_path, row.names = FALSE)

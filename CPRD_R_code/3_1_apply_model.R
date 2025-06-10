library(survival)
library(dplyr)
library(parallel)
library(arrow)
library(ggplot2)

# Load the model
model <- readRDS("N:/Desktop/yl_cam/UKB_model_bisw.rds")
print(model)

# Define the processing function
process_data <- function(index) {
  complete_dataset <- read_parquet(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/complete/complete_data_", index, ".parquet"))
  
  # Rename and select columns
  complete_dataset <- complete_dataset %>%
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
#  complete_dataset<-complete_dataset%>%filter(core_ethn_selfrep!="White")
  # Remove column
  complete_dataset$core_eligiable_screen <- NULL
  
  # Recode and relevel columns
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
  
  # Select applied data
  applied_data <- complete_dataset %>% select(-futime, -status)
  
  # Standardize continuous variables
  continuous_vars <- sapply(applied_data, is.numeric)
  applied_data[continuous_vars] <- scale(applied_data[continuous_vars])
  
  # Fit the survival model
  fit <- survfit(formula = model, newdata = applied_data, se.fit = FALSE)
  time <- fit$time
  surv <- fit$surv
  
  # Predict linear predictors
  lp <- predict(model, type = "lp", newdata = applied_data)
  
  # Create survival object
  surv_obj <- with(complete_dataset, Surv(futime, status))
  
  # Compute C-index
  concordance <- survConcordance(surv_obj ~ lp)
  predicted_probs <- surv[1464, ]
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
  
  print(paste("Calibration Intercept:", calibration_intercept))
  print(paste("95% CI for Intercept:", intercept_confint[1], "to", intercept_confint[2]))
  print(paste("Calibration Slope:", calibration_slope))
  print(paste("95% CI for Slope:", slope_confint[1], "to", slope_confint[2]))
  
  calibration_plot <- ggplot(calibration_data, aes(x = mean_predicted_risk, y = observed_probability)) +
   geom_point(color = "black") +
   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
   labs(x = "Predicted Risk (%)", y = "Observed Risk (%)", title = "Calibration (at 2 years) for the study population") +
   theme_minimal() +
   xlim(0, 1) +
   ylim(0, 1)
  
  ggsave(file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/cali_plot/calibration_plot_", index, ".png")), plot = calibration_plot, width = 8, height = 6)
  
  # Return results
  return(data.frame(
    index = index,
    concordance = concordance$concordance,
    intercept = calibration_intercept,
    intercept_lower = intercept_confint[1],
    intercept_upper = intercept_confint[2],
    slope = calibration_slope,
    slope_lower = slope_confint[1],
    slope_upper = slope_confint[2]
  ))
}

# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {
  library(survival)
  library(dplyr)
  library(arrow)
  library(ggplot2)
})

# Export necessary objects to cluster
clusterExport(cl, c("model", "process_data"))

# Process data for indexes 1 to 45 in parallel and collect results
results <- parLapply(cl, 1:45, process_data)

# Stop the cluster
stopCluster(cl)

# Combine results into a single data frame
results_df <- do.call(rbind, results)

# Save results to a CSV file
write.csv(results_df, "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/minor_race/evaluate_rare_race_results.csv", row.names = FALSE)

library(ggplot2)

load("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/applied_results/concordance_values.RData")

# Convert the concordance values to a data frame
concordance_df <- data.frame(
  Index = 1:length(concordance_values),
  Concordance = unlist(concordance_values)
)
summary(concordance_df$Concordance)
# Create the plot
p <- ggplot(concordance_df, aes(x = Index, y = Concordance)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_hline(yintercept = 0.727, linetype = "dashed", color = "green") +
  annotate("text", x = length(concordance_values) * 0.8, y = 0.73, label = "C-index on source data", color = "green", size = 5) +
  labs(title = "Concordance Values Across Datasets",
       x = "Dataset Index",
       y = "Concordance") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
print(p)



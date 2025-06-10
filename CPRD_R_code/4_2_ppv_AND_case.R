library(dplyr)
library(parallel)
library(arrow)

# Define the function to process data
process_data <- function(index) {
  complete_dataset <- read_parquet(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/complete/complete_data_", index, ".parquet"))
  complete_dataset<-complete_dataset%>%filter(core_eth!="White")%>%mutate(
    symp_ABDO_BLOAT_freq_in_lb=recode(symp_ABDO_BLOAT_freq_in_lb,"1+"=1),
    symp_ABDO_PAIN_freq_in_lb=recode(symp_ABDO_PAIN_freq_in_lb,"1+"=1),
    symp_STOM_DIS_freq_in_lb=recode(symp_STOM_DIS_freq_in_lb,"1+"=1))
  # Calculate the number of people with status == 1
  num_status_1 <- sum(complete_dataset$status == 1)
  
  # Select variables starting with biom, symp, medhist
  target_vars <- grep("^(biom|symp|medhist)", names(complete_dataset), value = TRUE)
  
  # Initialize the result dataframe
  result_df <- data.frame(
    index = integer(),
    num_status_1 = integer(),
    variable = character(),
    Total_Positive = integer(),
    Freq = numeric(),
    PP = integer(),
    PPV = numeric(),
    `PPV 95% CI` = character(),
    stringsAsFactors = FALSE
  )
  
  # Calculate PPV for each target variable
  for (var in target_vars) {
    total_positive <- sum(complete_dataset[[var]] == 1, na.rm = TRUE)
    freq <- total_positive / nrow(complete_dataset)
    pp <- sum(complete_dataset[[var]] == 1 & complete_dataset$status == 1, na.rm = TRUE)
    ppv <- pp / total_positive
    ci <- binom.test(pp, total_positive)$conf.int
    ci_low <- ci[1]
    ci_upp <- ci[2]
    
    result_df <- rbind(result_df, data.frame(
      index = index,
      num_status_1 = num_status_1,
      variable = var,
      Total_Positive = total_positive,
      Freq = freq,
      PP = pp,
      PPV = ppv,
      `PPV 95% CI` = sprintf("(%.4f, %.4f)", ci_low, ci_upp)
    ))
  }
  
  return(result_df)
}

# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {
  library(dplyr)
  library(arrow)
})

# Export necessary objects to the cluster
clusterExport(cl, c("process_data"))

# Process data for indexes 1 to 45 in parallel
results_list <- parLapply(cl, 1:45, process_data)

# Stop the cluster
stopCluster(cl)

# Combine all results into a single dataframe
all_results <- do.call(rbind, results_list)

# Save results to a CSV file
write.csv(all_results, "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/minor_race/ppv&case.csv", row.names = FALSE)


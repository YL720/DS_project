library(caret)
library(dplyr)
library(tibble)

col_names<-read.csv("Q:/rmjlico/df_colnames.csv")$x

project_data <- function(j) {

complete_index<-read_parquet(file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/complete/complete_data_", j, ".parquet")))

complete_index<-complete_index%>%rename(core_drinking_status=core_drinking_ever,
                                        core_smoking_status=core_smokingcat,
                                        core_sex_reported=core_sex,
                                        core_ethn_selfrep=core_eth,
                                        core_imd_fifth=core_imd,
                                        symp_all_CONS_new_onset=symp_CONS_new_onset,
                                        symp_all_DIARR_new_onset=symp_DIARR_new_onset,
                                        symp_all_HAEMR_new_onset=symp_HAEMR_new_onset,
                                        medhist_colonoscopy_ALL_in_10_yr_lb=medhist_colo_screen,
                                        medhist_bcscreen_eligible=core_eligiable_screen)%>%
                                select(-nelaa,-index,-core_yob)

complete_index$core_eligiable_screen<-NULL
complete_index<-complete_index%>%mutate(core_smoking_status=recode(core_smoking_status,
                                                                   "Current smoker"="current",
                                                                  "Ex smoker"="previous",
                                                                  "Non-smoker"="never"),
                                        core_smoking_status=relevel(core_smoking_status,ref='never'),
                                        core_ethn_selfrep=relevel(core_ethn_selfrep,ref='Other/Unknown'),
                                        core_imd_fifth=relevel(core_imd_fifth,ref=1))
df_outcome <- complete_index %>%
  select(eid, futime, status) %>%
  column_to_rownames(var = "eid")

df_num <- complete_index %>%
  select(eid, core_age, core_age2, core_bmi) %>%
  column_to_rownames(var = "eid")

df_factor <- complete_index %>%
  select(-futime, -status, -core_age, -core_age2, -core_bmi) %>%
  column_to_rownames(var = "eid")

str(df_outcome)
str(df_num)
str(df_factor)

columns_to_onehot <- names(sapply(df_factor, nlevels)[sapply(df_factor, nlevels) > 2])
dummy <- dummyVars(~ ., data = df_factor[, columns_to_onehot, drop = FALSE], fullRank = TRUE)
onehot_data <- predict(dummy, newdata = df_factor[, columns_to_onehot, drop = FALSE])

# Combine non-one-hot encoded and one-hot encoded data
df_factor_other <- df_factor[, !names(df_factor) %in% columns_to_onehot, drop = FALSE]
df_factor_onehot <- cbind(df_factor_other, onehot_data)

# Convert factors to numeric (0 and 1)
df_factor_numeric <- df_factor_onehot %>%
  mutate(across(where(is.factor), ~ as.numeric(.) - 1))

df_variable <- cbind(df_num, df_factor_numeric)
df_variable<-df_variable[,col_names]

normalized_random_direction_matrix <- as.matrix(read.csv("Q:/rmjlico/random_direction.csv"))
min_max_scale <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_variable_scaled <- as.data.frame(lapply(df_variable, min_max_scale))
num_variables<-ncol(df_variable_scaled)
projected_data_df2 <- as.matrix(df_variable_scaled) %*% t(normalized_random_direction_matrix)
df2_projected <- as.data.frame(projected_data_df2)

max_possible_value <- sqrt(num_variables)
min_possible_value <- -sqrt(num_variables)
steps <- seq(min_possible_value, max_possible_value, length.out = 500)

density_list <- apply(df2_projected, 2, function(column) {
  density(column, from = min_possible_value, to = max_possible_value, n = 500)
})

# Extract density values and x points
density_values <- sapply(density_list, function(d) d$y)
density_x <- density_list[[1]]$x

# Convert to data frame for easy handling
density_df <- data.frame(density_x, density_values)
str(density_df)

write.csv(density_df, file = file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/density/density_", j, ".csv")) )
}


library(parallel)
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(caret)
  library(dplyr)
  library(tibble)
  library(arrow)
})

clusterExport(cl, c("col_names"))


result <- parLapply(cl, 1:45, project_data)


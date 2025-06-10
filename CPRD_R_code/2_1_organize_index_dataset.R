library(parallel)
library(dplyr)
library(arrow)
library(purrr)
library(lubridate)
library(tidyr)

# ---- Define data processing function ----
process_data <- function(j) {
  index_date <- as.Date(index_dates[j, 1])
  message(paste("Started processing index:", j))
  
  # Load and clean baseline data
  df_baseline <- base::get(base::load(file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/baseline_variable/df_basevars_index_date", j, ".Rdata")))) %>%
    distinct() %>%
    select("eid", "drinking_ever", "smokingcat", "BMI") %>%
    rename_with(~ paste0("v0_", .), -eid)
  
  # Load and clean biomarker data
  df_biom <- base::get(base::load(file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/biomarkers/df_gp_biom_index_date_", j, ".Rdata")))) %>%
    select("eid", "iron_def", "iron_def_measured", "inflam_all", "inflam_measured") %>%
    distinct() %>%
    rename_with(~ paste0("biom_", .), -eid)
  
  # Load and clean colonoscopy screening record
  df_other_events <- base::get(base::load(file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/other_events/df_other_event", j, ".Rdata")))) %>%
    select("eid", "colo_screen") %>%
    distinct() %>%
    rename_with(~ paste0("other_", .), -eid)
  
  # Load and clean modifiers data
  df_modifier <- base::get(base::load(file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/modifier_events/df_gp_marker_events_index_date", j, ".Rdata")))) %>%
    select(eid, IBD_ever, DIB_T1D_ever, DIB_T2D_ever, GALL_ever, MED_NSAIDs_reg_ever, MED_ASPIRIN_reg_ever) %>%
    distinct() %>%
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    rename_with(~ paste0("mod_", .), -eid)
  
  # Load and clean marker data
  df_marker <- base::get(base::load(file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/marker_events/df_gp_marker_events_index_date", j, ".Rdata")))) %>%
    distinct() %>%
    rename_with(~ paste0("mrk_", .), -eid)
  
  # Load and clean people 
  survobj <- readRDS(sprintf("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_patient/pat_idx_%02d.rds", j)) %>%
    mutate(eid = e_patid) %>%
    select(-e_patid, -index_date) %>%
    distinct() %>%
    mutate(imd_5th = ceiling(imd2015_10 / 2)) %>%
    select(-imd2015_10)
  
  # Convert eid to character and combine datasets
  ls_df_names <- list(survobj, df_baseline, df_biom, df_other_events, df_modifier, df_marker)
  ls_dfs <- lapply(ls_df_names, function(df) {
    df <- df %>% mutate_at(vars(matches("^eid")), as.character)
    return(df)
  })
  
  # Merge all datasets
  coxdat <- ls_dfs %>% reduce(left_join, by = "eid") %>% distinct()
  coxdat <- coxdat %>%
    rename(age = age_at_index, v0_bmi = v0_BMI, status = crc_in_1_yr, v0_eth = gen_ethnicity, v0_sex = gender, v0_imd = imd_5th, v0_yob = yob) %>%
    mutate(age2 = age^2) %>%
    mutate(v0_eligiable_screen = ifelse(age >= 62 & age <= 76 & j >= 13, 1, 0)) %>%
    select(-deathdate, -diagnosisdate, -gp_end, -gp_start, -hes_eth)
  
  # NA means no recorded or Missing
  covars <- names(coxdat)[grepl("^v0_|^biom_|^mod_|^mrk_|^other_", names(coxdat))]
  coxdat <- coxdat %>% select(eid, futime, status, age, age2, all_of(covars))
  coxdat <- coxdat %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)),
           across(where(is.character), ~ replace_na(., 'Missing')))
  
  write_parquet(coxdat, file.path(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/coxdat/coxdat_index", j, "_back2", "_fwd1", ".parquet")))
}

# ---- Parallel processing setup ----
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
  library(purrr)
  library(arrow)
  library(tidyr)
})

clusterExport(cl, c("index_dates"))

result <- parLapply(cl, 1:45, process_data)

stopCluster(cl)

# ---- Combine all results ----
all_files <- list.files(path = "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_datasets/", full.names = TRUE)

stacked_data <- all_files %>%
  lapply(read_parquet) %>%
  bind_rows()

write_parquet(stacked_data, "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_dataset_back2_fwd1.parquet")

names_core <- c("age", "age2", "v0_yob", "v0_bmi", "v0_eth", "v0_sex", "v0_smokingcat", "v0_imd","v0_drinking_ever","v0_eligiable_screen")
names_medhist <- c("other_colo_screen","mod_IBD_ever", "mod_DIB_T1D_ever", "mod_DIB_T2D_ever", "mod_GALL_ever", "mod_MED_NSAIDs_reg_ever", "mod_MED_ASPIRIN_reg_ever")


process_data2 <- function(j) {
  index_data<-read_parquet(paste0("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/coxdat/coxdat_index", j, "_back2", "_fwd1", ".parquet"))
  names(index_data)[colSums(is.na(index_data))>0]
  colnames_df <- names(index_data)
  
  data.table::setnames(
    index_data,
    old = c(
      c("age", "age2"),
      names_core[!names_core %in% c("age", "age2")],
      names_medhist,
      colnames_df[grep("^mrk_", colnames_df)]
    ), 
    new = c(
      paste0("core_", c("age", "age2")),
      sub("^[^_]*_", "core_", names_core[!names_core %in% c("age", "age2")]),
      sub("^[^_]*_", "medhist_", names_medhist),
      sub("^[^_]*_", "symp_", colnames_df[grep("^mrk_", colnames_df)])
    )
  )
  
  index_data <- index_data %>%
    dplyr::select(-one_of(c(
      "medhist_DIB_T1D_ever", "symp_ABDO_LUMP_in_lb", "symp_RECTAL_MASS_in_lb",
      "symp_PELVIC_PAIN_freq_in_lb", "symp_WEIGHT_LOSS_new_onset", "symp_JAUNDICE_new_onset"
    )))  %>% 
    dplyr::mutate(
      core_eth = recode_factor(
        core_eth,
        "Chinese" = "Other/Unknown",
        "Missing"= "Other/Unknown",
        "Other" = "Other/Unknown"
      ),
      symp_ABDO_BLOAT_freq_in_lb = ifelse(symp_ABDO_BLOAT_freq_in_lb > 0, "1+", symp_ABDO_BLOAT_freq_in_lb),
      symp_ABDO_PAIN_freq_in_lb = ifelse(symp_ABDO_PAIN_freq_in_lb > 0, "1+", symp_ABDO_PAIN_freq_in_lb),
      symp_STOM_DIS_freq_in_lb = ifelse(symp_STOM_DIS_freq_in_lb > 0, "1+", symp_STOM_DIS_freq_in_lb)
    )
}
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
  library(purrr)
  library(arrow)
  library(tidyr)
})

clusterExport(cl, c("index_dates","names_core","names_medhist"))

result <- parLapply(cl, 1:45, process_data2)

stopCluster(cl)

all_files<-list.files(path="S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/index_datasets/", full.names = TRUE)

stacked_data <- all_files %>%
  lapply(read_parquet) %>% 
  bind_rows() 

table(stacked_data$index)
n_distinct(stacked_data$eid)
colnames(stacked_data)[colSums(is.na(stacked_data))>0]

write_parquet(stacked_data, "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_dataset_back2_fwd1.parquet")


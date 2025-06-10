source("~/DS_Project_Code/0_config.r")

coxdat_stacked_allages_num <- base::get(base::load(file.path("~/DS_project_data/coxdat_stacked_allages_num_20230726_PRScsx_CRC_OkSkinNM_LDpred_sansVision.Rdata"))) %>% 
  as.data.frame() %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  dplyr::select(-any_of(c("nelaa")))
str(coxdat_stacked_allages_num)

colnames_df <- names(coxdat_stacked_allages_num)

setnames(
  coxdat_stacked_allages_num,
  old = c(
    c("age", "age2"),
    names_core[!names_core %in% c("age", "age2")],
    names_medhist,
    grep("^mrk_", names(coxdat_stacked_allages_num), value = TRUE),
    names_lifestyle[!names_lifestyle %in% c("v0_tot_METs_wk")]
  ),
  new = c(
    paste0("core_", c("age", "age2")),
    sub("^[^_]*_", "core_", names_core[!names_core %in% c("age", "age2")]),
    sub("^[^_]*_", "medhist_", names_medhist),
    sub("^[^_]*_", "symp_", grep("^mrk_", names(coxdat_stacked_allages_num), value = TRUE)),
    sub("^[^_]*_", "lifestyle_", names_lifestyle[!names_lifestyle %in% c("v0_tot_METs_wk")])
  ),
  skip_absent = TRUE
)

if (grepl("OnlySymptomaticPop", tag)){
  coxdat_stacked_allages_num <- filter_coxdat_OnlySymptomaticPop(coxdat_stacked_allages_num, tag)
}
if (grepl("OnlySymptomaticPop_SA_AllSympSansFatigue", tag)){
  coxdat_stacked_allages_num <- filter_coxdat_SA_AllSympSansFatigue(coxdat_stacked_allages_num, tag)
}

if (grepl("sansVision", date_str)){
  eids_Vision <- fread(file.path("~/DS_project_data/eids_Vision.csv"))$eid
  coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>% dplyr::filter(! eid %in% eids_Vision)
}

data.table::setnames(
  coxdat_stacked_allages_num,
  old = c("v0_sex_reported", "v0_imd_fifth"),
  new = c("core_sex_reported", "core_imd_fifth")
)

coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>%
  dplyr::select(-one_of(c(
    "medhist_DIB_T1D_ever", "symp_ABDO_LUMP_in_lb", "symp_RECTAL_MASS_in_lb",
    "symp_PELVIC_PAIN_freq_in_lb", "symp_WEIGHT_LOSS_new_onset", "symp_JAUNDICE_new_onset"
  ))) %>%
  dplyr::mutate(
    core_ethn_selfrep = recode_factor(
      core_ethn_selfrep,
      "Chinese" = "Other/Unknown",
      "missing" = "Other/Unknown",
      "Other" = "Other/Unknown"
    ),
    symp_ABDO_BLOAT_freq_in_lb = ifelse(symp_ABDO_BLOAT_freq_in_lb > 0, "1+", symp_ABDO_BLOAT_freq_in_lb),
    symp_ABDO_PAIN_freq_in_lb = ifelse(symp_ABDO_PAIN_freq_in_lb > 0, "1+", symp_ABDO_PAIN_freq_in_lb),
    symp_STOM_DIS_freq_in_lb = ifelse(symp_STOM_DIS_freq_in_lb > 0, "1+", symp_STOM_DIS_freq_in_lb),
    lifestyle_alcohol_units_daily = ifelse(lifestyle_alcohol_units_daily > 0, 1, 0)
  )

binarycols <- coxdat_stacked_allages_num %>%
  dplyr::select_if(function(col) n_distinct(col) == 2) %>%
  dplyr::select(!status) %>%
  names()

factor_cols <- c(binarycols, "core_sex_reported", "lifestyle_alcohol_units_daily") %>% unique()

coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>%
  dplyr::mutate(across(all_of(factor_cols), function(col) {as.factor(col)}))

readable_names <- fread("~/DS_Project_Code/Dependency/dict_covariate_names.csv")
source("~/DS_Project_Code/Dependency/defn_data_sources.r")
readable_names <- readable_names %>%
  dplyr::mutate(data_source = case_when(
    covariate_code %in% names_core ~ "core",
    covariate_code %in% names_medhist ~ "medhist",
    covariate_code %in% names_lifestyle ~ "lifestyle",
    TRUE ~ sub("\\_.*", "", covariate_code)
  ))

eventless_cols <- eventless_riskfactor_levels(ind_symptomatic_only = ifelse(grepl("OnlySymptomaticPop", tag), TRUE, FALSE))

coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>%
  rename(core_drinking_status = lifestyle_alcohol_units_daily)

coxdat_stacked_allages_num$medhist_famhist_bowel_cancer <- NULL
coxdat_stacked_allages_num$medhist_famhist_breast_cancer <- NULL
coxdat_stacked_allages_num$medhist_famhist_lung_cancer <- NULL
coxdat_stacked_allages_num$medhist_bcscreen_eligible  <- NULL
coxdat_stacked_allages_num$core_birth_year<- NULL
str(coxdat_stacked_allages_num)

library(mlr3proba)
task = TaskSurv$new(
  id = "crc",
  backend = as.data.frame(coxdat_stacked_allages_num),
  time = "futime",
  event = "status"
)

task$set_col_roles("eid", add_to = "group")

cat("mlr3_pipeops_0_init -- task ......\n")
print(task)

po_scale <- po("scale")

source("~/DS_Project_Code/Dependency/si_PipeOpNelsonAalen.r")
po_nelaa <- si_PipeOpNelsonAalen$new()

po_encode <- po("encode")

po_select_rmnelaa <- po("select")
po_select_rmnelaa$param_set$values$selector = selector_invert(selector_name("nelaa"))


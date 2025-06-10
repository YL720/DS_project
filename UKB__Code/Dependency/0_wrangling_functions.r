get_baselinefields <- function(fpath_baseline, baseline_fields) {
  pheno_nums <- names(fread(
    fpath_baseline, 
    nrows=1
  ))
  
  ls_field_names <- lapply(
    split(baseline_fields, seq(nrow(baseline_fields))), 
    function(baseline_field) {
      field_nums <- pheno_nums[grepl(paste0("^", baseline_field$field, "-"), pheno_nums)]
      field_names <- gsub(baseline_field$field, baseline_field$desc, field_nums)
      return(field_names)
    }
  )
  ls_field_nums <- lapply(
    split(baseline_fields, seq(nrow(baseline_fields))), 
    function(baseline_field) {
      field_nums <- pheno_nums[grepl(paste0("^", baseline_field$field, "-"), pheno_nums)]
      return(field_nums)
    }
  )
  field_nums <- unname(unlist(ls_field_nums))
  field_names <- unname(unlist(ls_field_names))
  
  df_baseline <- fread(
    fpath_baseline, 
    select=c("eid", field_nums)
    # nrows=1000
  )
  names(df_baseline) <- c("eid", field_names)
  
  return(df_baseline)
}

collapse_cancer_records_1stcancerdeathdates <- function(df_cancer) {
  df_cancer <- df_cancer %>% mutate_at(vars(grep("date", names(df_cancer), value = TRUE)), funs(as.Date(.,"%Y-%m-%d")))
  
  df_cancer <- df_cancer %>% mutate(
    first_cancer_date = pmin(!!!rlang::syms(grep_colnames("cancer_date-", df_cancer)), na.rm=TRUE), 
    first_cancer_age = pmin(!!!rlang::syms(grep_colnames("cancer_age-", df_cancer)), na.rm=TRUE),
    death_date = pmin(!!!rlang::syms(grep_colnames("death_date-", df_cancer)), na.rm=TRUE),
    death_age = pmin(!!!rlang::syms(grep_colnames("death_age-", df_cancer)), na.rm=TRUE)
  ) 
  return(df_cancer)
}


grep_colnames <- function(col_string, df) {
  return(names(df)[grepl(col_string, names(df))])
}

get_powerset <- function(set) {
  n <- length(set)
  masks <- 2^(1:n-1)
  lapply( 1:2^n-1, function(u) set[ bitwAnd(u, masks) != 0 ] )
}

which_cols_hv_NAs <- function(df){
  return(names(which(colSums(is.na(df))>0)))
}


#--------------------------------- INSPECT ------------------------------------
# 4931 -- 2 entries
# df_cancer %>% filter(`cancer_date-1.0` == `cancer_date-0.0` )%>% filter(`cancer_type-0.0`  %in% medcodes_icd10)
# tmp[[which(df_cancer$eid == 4283308)]]
filter_coxdat_SA_AllSympSansFatigue <- function(coxdat_stacked_allages_num, tag){
  str(coxdat_stacked_allages_num)
  if (tag=="_OnlySymptomaticPop_SA_AllSympSansFatigue"){
    symp_selected <- grep("^symp_", colnames(coxdat_stacked_allages_num), value=TRUE)
    symp_selected <- symp_selected[!symp_selected %in% c("symp_FATIGUE_new_onset")]
    
    ind_symptomatic <- coxdat_stacked_allages_num %>% dplyr::select(all_of(c("eid", "core_age", symp_selected))) %>%
      dplyr::mutate(across(all_of(symp_selected), function(x) {as.numeric(as.character(x))}))  %>%
      dplyr::mutate(any_mrk = select(., all_of(symp_selected)) %>% rowSums(na.rm = TRUE), 
                    symptomatic=factor(1*(any_mrk >  0), levels=c("0", "1"))) 
    
    coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>% 
      left_join(ind_symptomatic %>% dplyr::select(eid,core_age,symptomatic), by = c("eid", "core_age")) %>%
      dplyr::filter(symptomatic=="1") %>%
      dplyr::select(!symptomatic)
    
    
    cat("N_symp: ", coxdat_stacked_allages_num$eid %>% unique() %>% length(), "; N_symp_cases: ",
        (coxdat_stacked_allages_num %>% filter(status==1))$eid %>% unique() %>% length(), "; dim: ", dim(coxdat_stacked_allages_num),"\n")
  }
  
  return(coxdat_stacked_allages_num)
}

filter_coxdat_OnlySymptomaticPop <- function(coxdat_stacked_allages_num, tag){
  if (tag=="_OnlySymptomaticPop"){
    graph <- readRDS(file=file.path(res_dir, "results", paste0("graph_1b_coxph_stepwise_bidirectional_selectsymptoms_agesex.rds"))) #HERE
    df_selectsymptoms_agesex <- graph$pipeops$si_surv.coxph_stepwise_bidirectional$learner_model$model %>% broom::tidy()
    terms_selectsymptoms_agesex <-  df_selectsymptoms_agesex$term
    if ("core_sex_genetic1" %in% terms_selectsymptoms_agesex) {
      readable_names <- fread("~/ACED-RREDD-EHR/sam/codelists/dict_covariate_names.csv")
      df_selectsymptoms_agesex <- df_selectsymptoms_agesex %>% dplyr::mutate(
        term=stringr::str_replace_all(term, setNames(paste0(readable_names$covar_code2, "."), readable_names$covar_code2)), 
        term=gsub("[.]$", "", term))  %>% 
        dplyr::mutate(term = ifelse(term == "core_age.2", "core_age2",term)) %>% 
        dplyr::mutate(term = ifelse(term == "biom_iron_def._measured1", "biom_iron_def_measured.1",term)) %>%
        dplyr::mutate(term_root = sub("\\..*", "", term)) 
    } 
    terms_selectsymptoms_agesex <-  df_selectsymptoms_agesex$term_root %>% unique()
    
    symp_selected <- terms_selectsymptoms_agesex[grep("^symp_", terms_selectsymptoms_agesex)] 
    cat("symp_selected......\n"); print(symp_selected)
    
    ind_symptomatic <- coxdat_stacked_allages_num %>% dplyr::select(all_of(c("eid", "core_age", symp_selected))) %>%
      dplyr::mutate(across(symp_selected, function(x) {as.numeric(as.character(x))}))  %>%
      dplyr::mutate(any_mrk = select(., all_of(symp_selected)) %>% rowSums(na.rm = TRUE), 
                    symptomatic=factor(1*(any_mrk >  0), levels=c("0", "1"))) 
    
    coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>% 
      left_join(ind_symptomatic %>% dplyr::select(eid,core_age,symptomatic), by = c("eid", "core_age")) %>%
      dplyr::filter(symptomatic=="1") %>%
      dplyr::select(!symptomatic)
    
    
    cat("N_symp: ", coxdat_stacked_allages_num$eid %>% unique() %>% length(), "; N_symp_cases: ",
        (coxdat_stacked_allages_num %>% filter(status==1))$eid %>% unique() %>% length(), "; dim: ", dim(coxdat_stacked_allages_num),"\n")
  }
  
  return(coxdat_stacked_allages_num)
}

my_round <- function(x, dp){format(round(x,dp), nsmall = dp) }
CI_my_round <- function(x, dp){
  x <- Rmisc::CI(x,ci=0.95)
  format(round(x,dp), nsmall = dp) }


education_dict <- c("1" = "College/University", "2" = "A/AS levels", "3" = "O levels/GCSEs", "4" = "CSEs", "5" = "NVQ/HND/HNC", "6" = "Other", "7" =  "None")
replace_education <- function(df, column_name, ind_abbrev) {
  # df <- df_hrs_mdls; column_name <- "covariate_readable";  ind_abbrev <- FALSE
  if (ind_abbrev){
    education_dict <- c("1" = "Col/Univ", "2" = "A/AS", "3" = "O/GCSE", "4" = "CSEs", "5" = "NVQ/HND/HNC", "6" = "Other", "7" =  "None")
  }
  # Function to replace the numbers with dictionary values
  replace_education_in_column <- function(x) {
    is_education <- str_detect(x, "^Education\\.")
    num <- str_extract(x[is_education], "\\d+$")
    new_value <- education_dict[num]
    x[is_education] <- ifelse(is.na(new_value), x[is_education], str_replace(x[is_education], paste0("\\.", num), paste0(".", new_value)))
    return(x)
  }
  
  # Convert specified column to character and apply the function
  df[[column_name]] <- as.character(df[[column_name]])
  df[[column_name]] <- replace_education_in_column(df[[column_name]])
  
  # Convert specified column back to factor with updated levels
  df <- df %>% dplyr::mutate(covariate_readable=factor(covariate_readable, levels=unique(df$covariate_readable)))
  return(df)
}



fmt_df_mdl_datasrc <- function(df_mdl, mdl){
  # df_mdl_cache <- df_mdl_1a_coxph
  # df_mdl <- df_mdl_1a_coxph
  # View(df_mdl %>% dplyr::select(term,term_root))
  # View(df_mdl)
  
  readable_names <- readable_names %>% dplyr::mutate(data_source=sub("\\_.*", "", covar_code2))
  df_mdl <- df_mdl %>% dplyr::mutate(
    sig = 1*(p.value <0.05),
    estimate_sign_sig = as.factor(sign(estimate)*sig)
  ) %>% dplyr::arrange(estimate_sign_sig, estimate)
  
  if ("core_sex_genetic1" %in% df_mdl$term) {
    df_mdl <- df_mdl %>% dplyr::mutate(
      term=stringr::str_replace_all(df_mdl$term, setNames(paste0(readable_names$covar_code2, "."), 
                                                          readable_names$covar_code2)),
      term=gsub("[.]$", "", term))
  } 
  df_mdl <- df_mdl %>% dplyr::mutate(term = ifelse(term == "core_age.2", "core_age2",term)) %>% 
    dplyr::mutate(term = ifelse(term == "biom_iron_def._measured1", "biom_iron_def_measured.1",term))
  
  #View(df_mdl)
  
  df_mdl <- df_mdl %>%  
    dplyr::mutate(
      # term_root = stringr::str_replace_all(df_mdl$term, setNames(paste0(readable_names$covariate_code, "."), readable_names$covariate_code)),
      term_root = sub("\\..*", "", term)
    ) %>%
    left_join(readable_names[, c("covar_code2", "data_source")], by = c("term_root" = "covar_code2")) %>% 
    left_join(readable_names[, c("covar_code2", "covariate_readable")], by = c("term_root" = "covar_code2")) %>% 
    dplyr::mutate(
      covariate_readable = coalesce(covariate_readable,term),
      # covariate_readable = gsub('v0_smoking_status', 'Smoking', covariate_readable),
      # covariate_readable = gsub('v0_ethn_selfrep', 'Ethnicity', covariate_readable),
      covariate_readable = stringr::str_replace_all(covariate_readable, setNames(readable_names$covariate_readable, readable_names$covariate_code)),
      data_source = coalesce(data_source, sub("\\_.*", "", term)),
      # data_source = gsub('other', 'medhist', data_source),
    )  
  
  df_mdl <- df_mdl %>% dplyr::filter(!term_root %in% (df_mdl$term_root[grep("^gen_pc", df_mdl$term_root)]))
  df_mdl <- df_mdl %>% dplyr::mutate(
    covariate_readable_root = covariate_readable,
    covariate_readable = paste0(covariate_readable, trimws(term, "left", "\\w"))
  ) %>% dplyr::mutate(
    covariate_readable = ifelse(term_root %in% binarycols, sub("\\..*", "", covariate_readable), covariate_readable)
  ) %>% dplyr::mutate(
    covariate_readable=ifelse(term_root=="core_sex_genetic", "Male (genetic)", covariate_readable)) #https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=22001
  df_mdl <- df_mdl %>% dplyr::mutate(color_datasource = case_when(
    df_mdl$data_source == "core" ~ "black", 
    df_mdl$data_source == "symp" ~ "#983B3B",
    df_mdl$data_source == "gen" ~ "#64734B",
    df_mdl$data_source == "lifestyle" ~ "#216974",
    df_mdl$data_source == "biom" ~ "#C27C1A",
    df_mdl$data_source == "medhist" ~ "#6D2952"
  ))
  
  
  # df_shap <- fread(file.path(res_dir, "results", paste0("df_shap_of_datasources_", date_str, "_", mdl, ".csv"))) %>% 
  #     dplyr::filter(data_source != "chance") %>% arrange(shap_val) 
  # df_shap <- df_shap %>% dplyr::mutate(data_source=factor(data_source, levels=rev(df_shap$data_source)))
  
  # cat("df_shap", mdl, "......\n")
  # print(df_shap)
  # print(levels(df_shap$data_source))
  
  
  df_mdl <-  df_mdl %>% 
    dplyr::mutate(data_source=ifelse(data_source=="mrk", "symp", data_source))%>% 
    # dplyr::mutate(data_source=factor(data_source, levels=levels(df_shap$data_source))) %>% 
    arrange(data_source,as.character(covariate_readable))%>% 
    dplyr::mutate(
      covariate_readable=factor(covariate_readable, levels=df_mdl$covariate_readable),
      data_source=factor(data_source, levels=unique(df_mdl$data_source)),
      color_datasource=factor(color_datasource, levels=unique(df_mdl$color_datasource))
    )
  
  
  cat("df_mdl", mdl, "......\n")
  print(levels(df_mdl$data_source))
  
  return(df_mdl)
}

eventless_riskfactor_levels <- function(ind_symptomatic_only){
  coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>% 
    dplyr::mutate_if(is.character, as.factor) %>% # %>% dplyr::mutate(eid=as.factor(eid))%>% as.data.frame() %>% 
    dplyr::select(-any_of(c("nelaa")))
  
  factor_cols <- c(binarycols, "core_smoking_status", "core_ethn_selfrep", "lifestyle_edu_quals", "symp_ABDO_BLOAT_freq_in_lb", "symp_ABDO_PAIN_freq_in_lb", "symp_PELVIC_PAIN_freq_in_lb", "symp_STOM_DIS_freq_in_lb") %>% intersect(.,colnames(coxdat_stacked_allages_num))
  coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>% dplyr::mutate(across(all_of(factor_cols), function(col) {as.factor(col)}))
  str(coxdat_stacked_allages_num)
  cases_onehot_count <- colSums(model.matrix(~., coxdat_stacked_allages_num %>% dplyr::filter(status==1) %>% dplyr::select(factor_cols)))
  df_eventless_cols <- data.frame(eventless_cols=cases_onehot_count[cases_onehot_count==0] %>% names())
  if ("core_sex_genetic1" %in% names(cases_onehot_count)) {
    df_eventless_cols <- df_eventless_cols %>% dplyr::mutate(
      eventless_cols=stringr::str_replace_all(df_eventless_cols$eventless_cols, setNames(glue::glue("{readable_names$covar_code2}."), 
                                                                                         readable_names$covar_code2)),
      eventless_cols=gsub("[.]$", "", eventless_cols))
  }
  
  return(df_eventless_cols$eventless_cols)
  
}


my_cap <- function(x) {
  x <- trimws(x)
  glue::glue("{toupper(substring(x, 1,1))}{substring(x, 2)}")
}
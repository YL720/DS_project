library(skimr)
library(dplyr)
library(tidyr)
library(data.table)
library(arrow)
source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
source("N:/Desktop/yl_cam/ACED_CODE/0_wrangling_functions.r")
stacked_data<-read_parquet("S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/stacked_dataset_back2_fwd1.parquet")

stacked_data<-stacked_data%>%filter(core_eth=="White")%>%filter(!is.na(core_imd))

lat<-stacked_data%>%
  group_by(eid)%>%filter(index==max(index))%>%ungroup()

summary(lat$core_drinking_ever)
summary(lat$core_bmi
        )
table(lat$status
      )
dir_table1 <- "S:/ECHO_IHI_CPRD/Data/Yangfan/pat_basic_data/table1s/WNW"

my_skim <- skim_with(factor=sfl(
  top_counts = ~top_counts(., max_char = 100),
  pct = ~{
    prt <- prop.table(table(.))
    nn <- table(.)
    perc <- my_round(prt*100, 1)
    nm1 <- names(prt)
    stringr::str_c(nm1, glue::glue("{nn} ({perc}%)"), sep = ": ", collapse = ",  ")
  }
))

pad_missing_levels <- function(string, levels=as.character(0:4)){
  # string <- "0: 2418 (100.0%),  1: 1 (  0.0%)"; levels<-as.character(0:2)
  level_info <- strsplit(string, ", ")[[1]] %>% trimws()
  existing_levels <- sapply(strsplit(level_info, ": "), function(x) x[1])
  missing_levels <- setdiff(levels, existing_levels) #Ok even if levels str, existing_levels int
  if (length(missing_levels) > 0) {
    missing_info <- paste0(missing_levels, ": 0 ( 0.0%)")
    string <- paste(string, paste(missing_info, collapse = ", "), sep=", ")
  }
  return(string)
}



get_CIl <- function(col){
  # https://bookdown.org/logan_kelly/r_practice/p09.html
  sample.n <- length(col)
  if (sample.n<=1){return(NA)}
  sample.mean <- mean(col)
  sample.sd <- sd(col)
  sample.se <- sample.sd/sqrt(sample.n)
  alpha = 0.05
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  margin.error <- t.score * sample.se
  CIl <- sample.mean - margin.error
  CIu <- sample.mean + margin.error
  return(CIl)
}

get_CIu <- function(col){
  # col <- df_evol %>% filter(core_age==41) %>% dplyr::select(medhist_mm_score_res_fit) %>% pull()
  # https://bookdown.org/logan_kelly/r_practice/p09.html
  sample.n <- length(col)
  if (sample.n<=1){return(NA)}
  sample.mean <- mean(col)
  sample.sd <- sd(col)
  sample.se <- sample.sd/sqrt(sample.n)
  alpha = 0.05
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  margin.error <- t.score * sample.se
  CIl <- sample.mean - margin.error
  CIu <- sample.mean + margin.error
  return(CIu)
}


# tbl1_colnames_var <- c("Predictor", "Level", "% (n) for categorical; Mean (SD) for continuous") #MEAN (SD)
tbl1_colnames_var <- c("Predictor", "Level", "N (%) for categorical; Median (IQR) for continuous") #MEDIAN (IQR)

tbl1_colname_counts <- grep("^N \\(\\%\\)", tbl1_colnames_var, value=TRUE)

# # CONSTANT: select constant features ----
vars <- paste0("^(", paste(c("eid", "status", "core"), collapse="|"), ")")
df_const <- select(stacked_data, matches(vars)) %>% dplyr::select(!c(core_age, core_age2,core_drinking_ever,core_smokingcat,core_bmi,core_eligiable_screen))

str(df_const)

print(colnames(df_const))
cat("num unique IDs (should 566174):", df_const$eid %>% unique() %>% length() , "\n")
cat("num distinct rows (566174 8):", df_const %>% dplyr::select(!status)%>% distinct() %>% dim() , "\n")


# CATEGORICAL ----
binarycols <- df_const %>% dplyr::select_if(
  function(col) { n_distinct(col) == 2}) %>% dplyr::select(-any_of(c("status","core_drinking_ever"))) %>% names()

vars_factor <- c(binarycols, "core_eth", "core_imd")%>% unique()
df_const <- df_const %>% dplyr::mutate(across(vars_factor, as.factor)) %>% 
  dplyr::mutate(across(all_of(grep("_famhist_", names(df_const), value=TRUE)), ~forcats::fct_recode(., `FALSE` = "0", `TRUE` = "1"))) 
df_const_factor <- df_const %>% dplyr::select(all_of(c("eid", vars_factor))) %>% distinct()
cat("df_const_factor......\n")
str(df_const_factor)

df_const_factor$core_sex_genetic <- recode(df_const_factor$core_sex, !!!c("2" = "Female", "1" = "Male"))


skimr_factor <- my_skim(df_const_factor %>% dplyr::select(all_of(c(vars_factor)))) %>% as.data.frame() %>% 
  dplyr::select(skim_variable,n_missing, complete_rate, factor.pct) %>%
  dplyr::rename(var=skim_variable, counts=factor.pct)
skimr_factor$Missing <- glue::glue("{skimr_factor$n_missing} ({my_round(100*(1-skimr_factor$complete_rate),1)}%)")

skimr_factor <- skimr_factor  %>% dplyr::select(c("var", "Missing","counts")) %>%
  dplyr::mutate(var_root = sub("^(.*?)_.*$", "\\1", var))

skimr_factor <- skimr_factor %>% 
  mutate(counts = strsplit(as.character(counts), ","))%>%
  unnest(counts) %>% as.data.frame()
skimr_factor$level <- do.call(rbind.data.frame, strsplit(skimr_factor$counts, split=":"))[,1]
skimr_factor$counts <- do.call(rbind.data.frame, strsplit(skimr_factor$counts, split=":"))[,2]

skimr_factor <- skimr_factor %>% 
  dplyr::filter(level!="FALSE") %>% 
  dplyr::mutate(var_clean = ifelse(duplicated(var), "", var), level=my_cap(level)) %>% 
  dplyr::select( var, var_clean, level, counts,Missing) 
cat("skimr_factor......\n")
skimr_factor

fwrite(skimr_factor, file.path(dir_table1, glue::glue("stacked_NW_table1_factor.csv")), row.names = FALSE)
cat("Done CATEGORICAL......\n")

# NUMERIC ----
vars_num <- colnames(df_const)[!colnames(df_const) %in% c(vars_factor, "eid", "status")]
df_const_num <- df_const %>% dplyr::select(all_of(c("eid", vars_num))) %>% distinct()

skimr_num <- my_skim(df_const_num %>% dplyr::select(all_of(c(vars_num)))) %>% as.data.frame()  %>% 
  dplyr::select(skim_variable, n_missing, complete_rate, numeric.mean, numeric.sd, numeric.p25, numeric.p50, numeric.p75, numeric.hist) %>%
  dplyr::rename(
    var=skim_variable
  ) %>%
  dplyr::mutate(across(where(is.numeric), round, 3))

# # MEAN (SD) -----
# skimr_num$mean_sd <- glue::glue("{skimr_num$numeric.mean %>% my_round(.,1)} ({skimr_num$numeric.sd %>% my_round(.,1)})")
# skimr_num <- skimr_num %>% dplyr::select(var, n_missing, complete_rate, mean_sd)

# MEDIAN (IQR) -----
skimr_num$median_iqr <- glue::glue("{skimr_num$numeric.p50 %>% my_round(.,1)} ({skimr_num$numeric.p25 %>% my_round(.,1)},{skimr_num$numeric.p75 %>% my_round(.,1)})")
skimr_num <- skimr_num %>% dplyr::select(var, n_missing, complete_rate, median_iqr)

skimr_num$Missing <- glue::glue("{my_round(100*(1-skimr_num$complete_rate),1)}% ({skimr_num$n_missing})")

# skimr_num <- skimr_num  %>% dplyr::select(c("var", "mean_sd")) #MEAN (SD)
skimr_num <- skimr_num  %>% dplyr::mutate(
  var_root = sub("^(.*?)_.*$", "\\1", var),
  var_clean = var
) %>%
  dplyr::select(c("var_root", "var", "var_clean", "median_iqr","Missing")) #MEDIAN (IQR)
print(skimr_num)

fwrite(skimr_num, file.path(dir_table1, glue::glue("stack_table1_NW_num.csv")), row.names = FALSE)
cat("Done NUMERIC......\n")

# Evolving EVER table --(1) Factors ----
cols_const <- colnames(df_const)[!colnames(df_const) %in% c("eid", "status")]
df_evol <- stacked_data %>% dplyr::select(-any_of(cols_const))
summary(df_evol)

df_evol_ever <- df_evol %>% dplyr::select(! c(
  "status", "futime", "core_age", "core_age2",
  colnames(df_evol)[grepl("^gen_pc",colnames(df_evol))], colnames(df_evol)[grepl("^gen_pc",colnames(df_evol))])) %>%
  dplyr::mutate_all(as.character) 
str(df_evol_ever)
df_evol_ever <- df_evol_ever %>%
  group_by(eid) %>%
  summarise(across(.cols = everything(), .fns = ~any(as.character(.) != "0"))) %>% as.data.frame() 

skimr_factor_evol <- my_skim(df_evol_ever %>% dplyr::select(!eid) %>% dplyr::mutate_all(as.factor)) %>% as.data.frame() %>% 
  dplyr::mutate(factor.pct = gsub(glue::glue("FALSE: {dim(df_evol_ever)[1]} \\(100\\.0%\\)"), 
                                  glue::glue("FALSE: {dim(df_evol_ever)[1]} \\(100\\.0%\\),  TRUE: 0 \\( 0\\.0%\\)"), factor.pct)) %>%
  dplyr::select(skim_variable,n_missing, complete_rate, factor.pct) %>%
  dplyr::rename(var=skim_variable, counts=factor.pct) %>% dplyr::mutate(var_clean=var,  var_root = sub("^(.*?)_.*$", "\\1", var)) %>% dplyr::select(c("var_root", "var", "var_clean", "counts")) %>% 
  dplyr::mutate(counts = strsplit(as.character(counts), ","))%>%
  unnest(counts) %>% as.data.frame() %>%
  dplyr::filter(!startsWith(counts, "FALSE:")) %>%
  dplyr::mutate( 
    counts = gsub("TRUE: ", "", counts),
    level = "≥1 record (any lm-age)"
  ) 

fwrite(skimr_factor_evol, file.path(dir_table1, glue::glue("stack_table1_NW_evolever.csv")), row.names = FALSE)


process_dataset <- function(data) {
  data<-data_list[[1]]
  index<-unique(data$index)
  data<-data%>%select(-index)%>%rename(medhist_eligiable_screen=core_eligiable_screen)
  my_skim <- skim_with(factor=sfl(
    top_counts = ~top_counts(., max_char = 100),
    pct = ~{
      prt <- prop.table(table(.))
      nn <- table(.)
      perc <- round(prt*100, 1)
      nm1 <- names(prt)
      stringr::str_c(nm1, glue::glue("{nn} ({perc}%)"), sep = ": ", collapse = ",  ")
    }
  ))
  
  pad_missing_levels <- function(string, levels=as.character(0:4)){
    level_info <- strsplit(string, ", ")[[1]] %>% trimws()
    existing_levels <- sapply(strsplit(level_info, ": "), function(x) x[1])
    missing_levels <- setdiff(levels, existing_levels)
    if (length(missing_levels) > 0) {
      missing_info <- paste0(missing_levels, ": 0 ( 0.0%)")
      string <- paste(string, paste(missing_info, collapse = ", "), sep=", ")
    }
    return(string)
  }
  
  vars <- paste0("^(", paste(c("eid", "status", "core"), collapse="|"), ")")
  df_const <- select(data, matches(vars)) %>% dplyr::select(!c(core_age, core_age2))
  
  binarycols <- df_const %>% dplyr::select_if(
    function(col) { n_distinct(col) == 2})  %>% dplyr::select(-any_of("status")) %>% names()
  
  vars_factor <- c(binarycols, "core_eth", "core_smokingcat","core_imd") %>% unique()
  df_const <- df_const %>% dplyr::mutate(across(vars_factor, as.factor)) %>% 
    dplyr::mutate(across(all_of(grep("_famhist_", names(df_const), value=TRUE)), ~forcats::fct_recode(., `FALSE` = "0", `TRUE` = "1"))) 
  df_const_factor <- df_const %>% dplyr::select(all_of(c("eid", vars_factor))) %>% distinct()
  
  df_const_factor$core_sex_genetic <- recode(df_const_factor$core_sex, !!!c("2" = "Female", "1" = "Male"))
  
  skimr_factor <- my_skim(df_const_factor %>% dplyr::select(all_of(c(vars_factor)))) %>% as.data.frame() %>% 
    dplyr::select(skim_variable,n_missing, complete_rate, factor.pct) %>%
    dplyr::rename(var=skim_variable, counts=factor.pct)
  skimr_factor$Missing <- glue::glue("{skimr_factor$n_missing} ({round(100*(1-skimr_factor$complete_rate),1)}%)")
  
  skimr_factor <- skimr_factor  %>% dplyr::select(c("var", "Missing","counts")) %>%
    dplyr::mutate(var_root = sub("^(.*?)_.*$", "\\1", var))
  
  skimr_factor <- skimr_factor %>% 
    mutate(counts = strsplit(as.character(counts), ",")) %>%
    unnest(counts) %>% as.data.frame()
  skimr_factor$level <- do.call(rbind.data.frame, strsplit(skimr_factor$counts, split=":"))[,1]
  skimr_factor$counts <- do.call(rbind.data.frame, strsplit(skimr_factor$counts, split=":"))[,2]
  
  skimr_factor <- skimr_factor %>% 
    dplyr::filter(level!="FALSE") %>% 
    dplyr::mutate(var_clean = ifelse(duplicated(var), "", var), level=str_to_title(level)) %>% 
    dplyr::select( var, var_clean, level, counts,Missing) 
  
  fwrite(skimr_factor, file.path(dir_table1, glue::glue("index/table1_factor{index}.csv")), row.names = FALSE)
  
  vars_num <- colnames(df_const)[!colnames(df_const) %in% c(vars_factor, "eid", "status")]
  df_const_num <- df_const %>% dplyr::select(all_of(c("eid", vars_num))) %>% distinct()
  
  skimr_num <- my_skim(df_const_num %>% dplyr::select(all_of(c(vars_num)))) %>% as.data.frame()  %>% 
    dplyr::select(skim_variable, n_missing, complete_rate, numeric.mean, numeric.sd, numeric.p25, numeric.p50, numeric.p75, numeric.hist) %>%
    dplyr::rename(
      var=skim_variable
    ) %>%
    dplyr::mutate(across(where(is.numeric), round, 3))
  
  skimr_num$median_iqr <- glue::glue("{round(skimr_num$numeric.p50,1)} ({round(skimr_num$numeric.p25,1)},{round(skimr_num$numeric.p75,1)})")
  skimr_num <- skimr_num %>% dplyr::select(var, n_missing, complete_rate, median_iqr)
  
  skimr_num$Missing <- glue::glue("{round(100*(1-skimr_num$complete_rate),1)}% ({skimr_num$n_missing})")
  
  skimr_num <- skimr_num  %>% dplyr::mutate(
    var_root = sub("^(.*?)_.*$", "\\1", var),
    var_clean = var
  ) %>%
    dplyr::select(c("var_root", "var", "var_clean", "median_iqr","Missing"))
  
  fwrite(skimr_num, file.path(dir_table1, glue::glue("index/table1_num{index}.csv")), row.names = FALSE)
  
  cols_const <- colnames(df_const)[!colnames(df_const) %in% c("eid", "status")]
  df_evol <- data %>% dplyr::select(-any_of(cols_const))
  
  df_evol_ever <- df_evol %>% dplyr::select(! c(
    "status", "futime", "core_age", "core_age2",
    colnames(df_evol)[grepl("^gen_pc",colnames(df_evol))], colnames(df_evol)[grepl("^gen_pc",colnames(df_evol))])) %>%
    dplyr::mutate_all(as.character) 
  
  df_evol_ever <- df_evol_ever %>%
    group_by(eid) %>%
    summarise(across(.cols = everything(), .fns = ~any(as.character(.) != "0"))) %>% as.data.frame() 
  
  skimr_factor_evol <- my_skim(df_evol_ever %>% dplyr::select(!eid) %>% dplyr::mutate_all(as.factor)) %>% as.data.frame() %>% 
    dplyr::mutate(factor.pct = gsub(glue::glue("FALSE: {dim(df_evol_ever)[1]} \\(100\\.0%\\)"), 
                                    glue::glue("FALSE: {dim(df_evol_ever)[1]} \\(100\\.0%\\),  TRUE: 0 \\( 0\\.0%\\)"), factor.pct)) %>%
    dplyr::select(skim_variable,n_missing, complete_rate, factor.pct) %>%
    dplyr::rename(var=skim_variable, counts=factor.pct) %>% dplyr::mutate(var_clean=var,  var_root = sub("^(.*?)_.*$", "\\1", var)) %>% dplyr::select(c("var_root", "var", "var_clean", "counts")) %>% 
    dplyr::mutate(counts = strsplit(as.character(counts), ","))%>%
    unnest(counts) %>% as.data.frame() %>%
    dplyr::filter(!startsWith(counts, "FALSE:")) %>%
    dplyr::mutate( 
      counts = gsub("TRUE: ", "", counts),
      level = "≥1 record (any lm-age)"
    ) 
  
  fwrite(skimr_factor_evol, file.path(dir_table1, glue::glue("index/table1_evolever{index}.csv")), row.names = FALSE)
}

cl <- makeCluster(detectCores() - 1) # Use one less core than available
clusterExport(cl, varlist = c("process_dataset", "dir_table1"))
clusterEvalQ(cl, {library(data.table)
  library(dplyr)
                  library(skimr)
                  library(tidyr) 
                  library(stringr)
                  source("N:/Desktop/yl_cam/ACED_CODE/gp_data_funcs.r")
                  source("N:/Desktop/yl_cam/ACED_CODE/0_wrangling_functions.r")})


data_list <- split(stacked_data, stacked_data$index)
parLapply(cl, data_list, process_dataset)
stopCluster(cl)

stacked_data<-stacked_data%>%arrange(eid,desc(index))%>%select(eid,index,core_smokingcat)
lat_smo<-stacked_data%>%group_by(eid)%>%slice(1)%>%ungroup()


crc_pat<-stacked_data%>%filter(status==1)%>%select(eid)%>%distinct()

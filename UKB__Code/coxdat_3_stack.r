source("~/DS_Project_Code/0_config.r")

coxdat_stacked_allages_num <- lapply(as.list(lmas), function(lma) {
  read_parquet(file.path("~/DS_project_data", paste0("coxdat/coxdat_lma", lma, "_back", num_yrs_back, "_fwd", num_yrs_fwd, ".parquet")))
}) %>% rbindlist()

str(coxdat_stacked_allages_num)
nrow(coxdat_stacked_allages_num) #1291542
sort(names(coxdat_stacked_allages_num))

# any unexpected NAs? ----
cols_with_NAs <- which_cols_hv_NAs(coxdat_stacked_allages_num)
cat("cols_with_NAs......\n", paste(sort(cols_with_NAs), collapse=", "), "\n")

# Complete Case
coxdat_stacked_allages_num <- coxdat_stacked_allages_num %>% 
  dplyr::filter(complete.cases(.))
nrow(coxdat_stacked_allages_num) #1287964
# make sure no withdrawn participants ----
ids_withdrawn <- fread("~/DS_project_data/w28126_20220222.csv")
if (any(coxdat_stacked_allages_num$eid %in% ids_withdrawn$V1)) stop("Contains withdrawn participants\n")

save(coxdat_stacked_allages_num, file=file.path("~/DS_project_data", paste0("coxdat_stacked_allages_num_", date_str, ".Rdata")))

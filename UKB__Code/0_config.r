library(dplyr)
sessionInfo() %>% print()

tag <- "" # get("tag", envir = .GlobalEnv) "", _OnlySymptomaticPop, _OnlySymptomaticPop_SA_AllSympSansFatigue
tag_Vision <- "_sansVision" #get("tag_Vision", envir = .GlobalEnv) _sansVision
tag_LDpred <-"_LDpred"  # get("tag_LDpred", envir = .GlobalEnv) _LDpred

vec_libraries <- c("arrow", "bbotk", "checkmate", "data.table", "DiceKriging", "dplyr", "forestmodel",   #"caret", 
                   "GGally", "ggplot2", "ggpubr", "gtools", "lubridate",  #"future", "rlang",
                   "mlr3", "mlr3learners", "mlr3tuning", "mlr3pipelines", #"mlr3verse", "mlr3proba", "mlr3mbo", 
                   "paradox",  "readxl", "reshape2", #"ranger","scales", 
                   "stringi", "survival", "tidyverse", "zoo")
needed_libraries <- vec_libraries[!(vec_libraries %in% installed.packages()[,"Package"])]
cat("\n\nmissing but needed libraries......\n")
needed_libraries %>% print()
# chooseCRANmirror(ind = 75)
if(length(needed_libraries)>0) {utils::install.packages(needed_libraries, dependencies = TRUE, INSTALL_opts = '--no-lock')}

lapply(as.list(needed_libraries), function(lib) {
  install.packages(lib, character.only = TRUE)
})

ls_library <- as.list(vec_libraries)
lapply(ls_library, function(lib) {
  library(lib, character.only = TRUE)
})
remotes::install_github("mlr-org/mlr3proba")
remotes::install_github("miraisolutions/godmode", build_opts = "")
cat("\n\nPackages loaded ......\n")

date_str<-"20230726_PRScsx_CRC_OkSkinNM_LDpred_sansVision"

r_files <- list.files("~/DS_Project_Code/Dependency", pattern = "\\.[rR]$", full.names = TRUE)

for (file in r_files) {
  cat("Sourcing:", file, "\n")
  tryCatch(
    source(file),
    error = function(e) {
      cat("❗ Error in", file, "\n")
      cat("Message:", e$message, "\n\n")
    }
  )
}



################################################################################
#' @description Create codebook for StudyDatabase2023
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(readxl)
#' Inputs
source("./src/set-inputs.R")
## Study database
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!(grepl("noMal", dat_filename, ignore.case = TRUE))] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
if(ageSexSuffix %in% c("01to59m")){
  dat_filename <- list.files("./gen/create-studydb/output")
  dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  dat_filename <- dat_filename[(grepl("noMal", dat_filename, ignore.case = TRUE))] 
  dat_filename <- tail(sort(dat_filename),1) # Most recent
  datNoMal <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
}
## Calibrated study data
dat_filename <- list.files("./data/study-data-calibrated")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!(grepl("noMal", dat_filename, ignore.case = TRUE))] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat_cal <- read_excel(paste0("./data/study-data-calibrated/", dat_filename, sep = ""))
if(ageSexSuffix %in% c("01to59m")){
  dat_filename <- list.files("./data/study-data-calibrated")
  dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  dat_filename <- dat_filename[(grepl("noMal", dat_filename, ignore.case = TRUE))] 
  dat_filename <- tail(sort(dat_filename),1) # Most recent
  datNoMal_cal <- read_excel(paste0("./data/study-data-calibrated/", dat_filename, sep = ""))
}
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
df_reclass <- subset(key_cod, !is.na(cod_reclass))
# Exclude "TB" which has been redistributed (only present in key for 5-9y and 10-14y)
df_reclass <- subset(df_reclass, cod_reclass != "TB")
# Exclude "Undetermined" which is used to eliminate studies in cleaning phase
df_reclass <- subset(df_reclass, cod_reclass != "Undetermined")
v_cod <- sort(unique(df_reclass$cod_reclass))

if(nrow(dat) != nrow(dat_cal)){
  warning("Study data and calibrated data have different number of rows.")
  # This is ok for neonates because we dropped 18 GBD and LiST data points after sharing with Sandy.
  # For 1-59m it should be the same number of data points.
}
if(ncol(dat) != ncol(dat_cal)){
  warning("Study data and calibrated data have different number of rows.")
}

v_col_names <- names(dat)

cal <- dat_cal %>%
  select(all_of(c("strata_id", v_cod))) %>%
  right_join(dat, by = "strata_id", suffix = c("", "_old")) %>% 
  select(-ends_with("_old")) %>%
  select(all_of(v_col_names)) %>%
  arrange(recnr)

if(ageSexSuffix %in% c("01to59m")){
  
  v_col_names <- names(datNoMal)
  
  calNoMal <- datNoMal_cal %>%
    select(all_of(c("strata_id", v_cod))) %>%
    right_join(datNoMal, by = "strata_id", suffix = c("", "_old")) %>% 
    select(-ends_with("_old")) %>%
    select(all_of(v_col_names)) %>%
    arrange(recnr)
}

# Save outputs ------------------------------------------------------------

write.csv(cal, paste0("./gen/create-studydb/output/StudyDatabase2023_",ageSexSuffix,"-calibrated_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)
if(ageSexSuffix %in% c("01to59m")){
  write.csv(calNoMal, paste0("./gen/create-studydb/output/StudyDatabase2023_",ageSexSuffix,"-calibrated-noMalnutrition_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)
}

################################################################################
#' @description 
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
dat_cal <- read.csv(paste0("./data/study-data-calibrated/", dat_filename, sep = ""))
if(ageSexSuffix %in% c("01to59m")){
  dat_filename <- list.files("./data/study-data-calibrated")
  dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  dat_filename <- dat_filename[(grepl("noMal", dat_filename, ignore.case = TRUE))] 
  dat_filename <- tail(sort(dat_filename),1) # Most recent
  datNoMal_cal <- read.csv(paste0("./data/study-data-calibrated/", dat_filename, sep = ""))
}
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key_cod, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]
# Exclude "Undetermined" for certain checks
v_cod_noundt <- v_cod_reclass[!(v_cod_reclass %in% "Undetermined")]

if(nrow(dat) != nrow(dat_cal)){
  warning("Study data and calibrated data have different number of rows.")
  # This is because we dropped GBD, LiST, and CHAMPS data points after sharing with Sandy.
  # Also dropped small adhoc studies from India.
  # Study db will have fewer data points compared to calibration on 20250214 for 1-59m and 20250218 for neonates
}

# Save column names of study data
v_col_names <- names(dat)

# Adjust strata_id for adhoc studies in calibrated data
# Updated these strata_id to make more consistent with other study data points after sharing with Sandy (woops)
# dat_cal$strata_id[grepl("adhoc", dat_cal$strata_id, ignore.case = TRUE)] <-
#   gsub("adHoc", "", dat_cal$strata_id[grepl("adhoc", dat_cal$strata_id, ignore.case = TRUE)])
# if(ageSexSuffix %in% c("01to59m")){
#   datNoMal_cal$strata_id[grepl("adhoc", datNoMal_cal$strata_id, ignore.case = TRUE)] <-
#     gsub("adHoc", "", datNoMal_cal$strata_id[grepl("adhoc", datNoMal_cal$strata_id, ignore.case = TRUE)])
# }
# Note 2025-04-03
# no longer need because Sandy has already calibrated with new strata_ids

# Identify new strata_ids that are not in calibrated data
v_new <- subset(dat, !(strata_id %in% dat_cal$strata_id))$strata_id
if(ageSexSuffix %in% c("01to59m")){
  v_new2 <- subset(dat, !(strata_id %in% datNoMal_cal$strata_id))$strata_id
  v_new <- unique(c(v_new, v_new2))
}
dat_filename <- list.files("./data/study-data-calibrated")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!(grepl("noMal", dat_filename, ignore.case = TRUE))] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
studydbver_audited <- sub("^[^_]+_[^_]+_([^.]*)\\.csv$", "\\1", dat_filename)
audit_message <- paste(
  paste0("New strata_id that are not in ", dat_filename,
         "\n-----------------\n"),
  paste(v_new, collapse = "\n")
)
audit_file <- paste0("./gen/create-studydb/audit/new-strata_id_",ageSexSuffix,"_", format(Sys.Date(), format="%Y%m%d"),".txt")
writeLines(audit_message, audit_file)


# # Merge calibrated COD onto studies
# # Remove old COD columns
# dat_new <- dat_cal %>%
#   select(all_of(c("strata_id", v_cod_noundt))) %>%
#   mutate(flag = "Calibrated") %>%
#   right_join(dat, by = "strata_id", suffix = c("", "_old")) %>% 
#   select(-ends_with("_old")) %>%
#   select(all_of(c(v_col_names, "flag"))) %>%
#   arrange(recnr)
# 
# if(ageSexSuffix %in% c("01to59m")){
#   
#   v_col_names <- names(datNoMal)
#   
#   datNoMal_new <- datNoMal_cal %>%
#     select(all_of(c("strata_id", v_cod_nondt))) %>%
#     mutate(flag = "Calibrated") %>%
#     right_join(datNoMal, by = "strata_id", suffix = c("", "_old")) %>% 
#     select(-ends_with("_old")) %>%
#     select(all_of(v_col_names)) %>%
#     arrange(recnr)
# }
# 
# 
# # Check if any data points were not present in calibrated data
# if(nrow(subset(dat_new, is.na(flag))) > 0){
#   warning("Some data points did not merge with calibrated dataset.")
# }
# dat_new$flag <- NULL
# 
# if(ageSexSuffix %in% c("01to59m")){
#   if(nrow(subset(datNoMal_new, is.na(flag))) > 0){
#     warning("Some data points in noMal dataset did not merge with calibrated dataset.")
#   }
#   datNoMal_new$flag <- NULL
# }
# 
# 
# # Save outputs ------------------------------------------------------------
# 
# write.csv(dat_new, paste0("./gen/create-studydb/output/StudyDatabase2023_",ageSexSuffix,"-calibrated_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)
# if(ageSexSuffix %in% c("01to59m")){
#   write.csv(datNoMal_new, paste0("./gen/create-studydb/output/StudyDatabase2023_",ageSexSuffix,"-calibrated-noMalnutrition_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)
# }

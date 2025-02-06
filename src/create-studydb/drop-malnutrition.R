################################################################################
#' @description Create version of StudyDatabase2023 without malnutrition for 1-59m
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
## Study db
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
################################################################################

dat$Other <- ifelse(!is.na(dat$Malnutrition) & !is.na(dat$Other), dat$Other + dat$Malnutrition, dat$Other)
dat$Other <- ifelse(!is.na(dat$Malnutrition) & is.na(dat$Other), dat$Malnutrition, dat$Other)
dat$Malnutrition <- NULL

# Save outputs ------------------------------------------------------------

write.csv(dat, paste0("./gen/create-studydb/output/StudyDatabase2023_",ageSexSuffix,"-noMalnutrition_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)

################################################################################
#' @description Pull inputs from Data Warehouse
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(readstata13)
require(stringr) # for fn_intEnv
#' Inputs
source("./src/set-inputs.R")
#' Functions 
source("./src/add-study-covar/fn_initEnvironment.R")
################################################################################

# Prediction database
fn_initEnvironment("prediction-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/databases/prediction-database", sep = ""))
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
dat_pred <- read.csv(paste0(pathDataWarehouse, "/2000-2023/databases/prediction-database/", dat_filename, sep = ""))
write.csv(dat_pred, paste0("./data/prediction-database/",dat_filename , sep = ""), row.names = FALSE)

# Covariate data extraction from DHS
fn_initEnvironment("dhs")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/databases/dhs-covariate-extraction", sep = ""))
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
dat_dhs <- read.csv(paste0(pathDataWarehouse, "/2000-2023/databases/dhs-covariate-extraction/", dat_filename, sep = ""))
write.csv(dat_dhs, paste0("./data/dhs/",dat_filename , sep = ""), row.names = FALSE)

# Matched study and DHS regions
fn_initEnvironment("classification-keys/dhs-regions-for-studies")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/classification-keys", sep = ""))
dat_filename <- dat_filename[grepl("manually-added", dat_filename, ignore.case = TRUE)] # StudyDHSregionMatching_manually-added-regions_20241002_AW.csv
dat_dhs <- read.csv(paste0(pathDataWarehouse, "/2000-2023/data/classification-keys", dat_filename, sep = ""))
write.csv(dat_dhs, paste0("./data/classification-keys/dhs-regions-for-studies/",dat_filename , sep = ""), row.names = FALSE)
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/classification-keys", sep = ""))
dat_filename <- dat_filename[grepl("studydhsregionmatching", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[!grepl("manually-added", dat_filename, ignore.case = TRUE)]
dat_dhs <- read.csv(paste0(pathDataWarehouse, "/2000-2023/data/classification-keys/", dat_filename, sep = ""))
write.csv(dat_dhs, paste0("./data/classification-keys/dhs-regions-for-studies/",dat_filename , sep = ""), row.names = FALSE)



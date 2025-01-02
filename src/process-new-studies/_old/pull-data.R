################################################################################
#' @description Pull inputs from Data Warehouse
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
#' Inputs
source("./src/set-inputs.R")
#' Functions 
source("./src/process-new-studies/fn_initEnvironmentData.R")
################################################################################

# Prediction database
fn_initEnvironment("prediction-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/databases/prediction-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/databases/prediction-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/prediction-database/",dat_filename[i]))
}

# Old model inputs
fn_initEnvironmentData("model-inputs-old")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/data/model-inputs", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2021/data/model-inputs/",dat_filename[i], sep = ""),
            to   = paste0("./data/model-inputs-old/",dat_filename[i]))
}

# New study data
fn_initEnvironmentData("study-data")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/study-data", sep = ""))
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/study-data/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-data/",dat_filename[i]))
}

# Single causes database
fn_initEnvironmentData("single-causes")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/databases/single-causes", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/databases/single-causes/",dat_filename[i], sep = ""),
            to   = paste0("./data/single-causes/",dat_filename[i]))
}

# Classification keys
fn_initEnvironmentData("classification-keys")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/classification-keys", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/classification-keys/",dat_filename[i], sep = ""),
            to   = paste0("./data/classification-keys/",dat_filename[i]))
}

# Covariate data extraction from DHS
fn_initEnvironmentData("dhs")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/databases/dhs-covariate-extraction", sep = ""))
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/databases/dhs-covariate-extraction/",dat_filename[i], sep = ""),
            to   = paste0("./data/dhs/",dat_filename[i]))
}

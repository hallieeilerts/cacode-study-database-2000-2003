################################################################################
#' @description Pull inputs from Data Warehouse
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
#' Functions 
source("./src/fn_initEnvironmentData.R")
################################################################################


# Prediction database
fn_initEnvironmentData("prediction-database")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/prediction-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/prediction-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/prediction-database/",dat_filename[i]))
}

# India subnational prediction database
fn_initEnvironmentData("prediction-database-india")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/model-pipeline/prediction-database-india", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2021/model-pipeline/prediction-database-india/",dat_filename[i], sep = ""),
            to   = paste0("./data/prediction-database-india/",dat_filename[i]))
}

# Old study data
fn_initEnvironmentData("study-data-old")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/model-pipeline/study-database", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2021/model-pipeline/study-database/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-data-old/",dat_filename[i]))
}

# Old study data with new variables extracted (VA alogirthms, age bounds)
fn_initEnvironmentData("study-data-old-augmented")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/reextracted-var-2000-2019-studies", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/reextracted-var-2000-2019-studies/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-data-old-augmented/",dat_filename[i]))
}

# Old model inputs
fn_initEnvironmentData("model-inputs-old")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2021/model-pipeline/model-inputs", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2021/model-pipeline/model-inputs/",dat_filename[i], sep = ""),
            to   = paste0("./data/model-inputs-old/",dat_filename[i]))
}

# New study data
fn_initEnvironmentData("study-data")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/study-data", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/study-data/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-data/",dat_filename[i]))
}

# Ad hoc data
fn_initEnvironmentData("ad-hoc")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/ad-hoc", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/ad-hoc/",dat_filename[i], sep = ""),
            to   = paste0("./data/ad-hoc/",dat_filename[i]))
}

# Calibrated study data
fn_initEnvironmentData("study-data-calibrated")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/calibration", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/calibration/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-data-calibrated/",dat_filename[i]))
}

# Single causes database
fn_initEnvironmentData("single-causes")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/model-pipeline/single-causes", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/model-pipeline/single-causes/",dat_filename[i], sep = ""),
            to   = paste0("./data/single-causes/",dat_filename[i]))
}

# Classification keys
fn_initEnvironmentData("classification-keys")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/keys", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/keys/",dat_filename[i], sep = ""),
            to   = paste0("./data/classification-keys/",dat_filename[i]))
}

# Covariate data extraction from DHS
fn_initEnvironmentData("dhs")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/dhs-covariate-extraction", sep = ""))
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/dhs-covariate-extraction/",dat_filename[i], sep = ""),
            to   = paste0("./data/dhs/",dat_filename[i]))
}

# pfpr covariate data
fn_initEnvironmentData("study-pfpr")
dat_filename <- list.files(paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/study-pfpr", sep = ""))
dat_filename <- dat_filename[grepl("pfpr_map", dat_filename, ignore.case = TRUE)]
for(i in 1:length(dat_filename)){
  file.copy(from = paste0(pathDataWarehouse, "/2000-2023/data/study-database-supplements/study-pfpr/",dat_filename[i], sep = ""),
            to   = paste0("./data/study-pfpr/",dat_filename[i]))
}

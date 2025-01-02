################################################################################
#' @description Merge on deaths, save columns in correct order
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
## Old age-specific study database that now has covariate names and scales as pred database
studydb <- read.csv(paste0("./gen/update-covar-for-old-studies/temp/studies_upd-covar_", ageSexSuffix, ".csv", sep = ""))
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-covar-for-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key <- read.csv(paste0("./gen/update-covar-for-old-studies/output/", dat_filename, sep = ""))
## Age-specific inputs
# For under-5 getting deaths straight from unprocessed study database
# For 5-19 using old model inputs, because these were subject to a lot of processing
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  # Old study database that has death columns
  dat_filename <- list.files("./data/study-data-old")
  dat_filename <- dat_filename[grepl("combined", dat_filename)]
  deaths <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
}
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){
  ## Old model input for deaths that now has updated COD names
  load(paste("./gen/update-covar-for-old-studies/output/ModInput2020-Deaths_", ageSexSuffix,".RData", sep = ""))
}
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

if(ageSexSuffix %in% c("00to28d", "01to59m")){
  
  # COD columns for under-5s from old study database
  v_cod <- c("totdeaths", "injuries", "malaria", "measles", "meningitis","neonatal", 
                 "other_nomaln","pneumonia", "malnutrition", "unknown", "hiv", "congenital", "other",
                 "pdia","pinj","pmal" , "pmen", "pneo", "poth", "ppne", "pcon", "ptot", "diarrhea")
  deathsWide <- deaths[,c("study_id", v_cod)]
  names(deathsWide)[names(deathsWide) == "study_id"] <- "id"
  
  # Check that same number of rows in studydb with updated covariates as there originally were
  nrow(deathsWide) == nrow(studydb)
  # Check that all the ids are the same
  any(deathsWide$id != studydb$id)
}
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){
  # Delete totdeaths because it should come from deaths in model input
  v_delete <- c("totdeaths")
  studydb <- studydb[!(names(studydb) %in% v_delete)]
  # Reshape deaths wide
  deathsWide <- deaths %>%
    select(id, totdeaths, cause, n) %>%
    pivot_wider(names_from = cause, values_from = n) 
  # Vector with all CODs in correct order
  df_cod <- key_cod[,c("cod_05to19","cod_order")]
  df_cod <- df_cod[order(df_cod$cod_order),]
  v_cod <- unique(df_cod$cod_05to19)
  # Identify cod columns in model input
  v_cod <- v_cod[v_cod %in% names(deathsWide)]
}

# Merge deaths with study database
dat <- merge(studydb, deathsWide, by = "id")

# Covariate columns
v_covar <- unique(key$pred[key$pred %in% names(dat)])

# Plus meningitis_epi for 0-1m and 1-59m
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  v_covar <- c(v_covar, "meningitis_epi")
}

# Source columns
v_src <- paste0(v_covar, "_source", sep = "")

# Combined vector of covariate and source columns
v_covarsrc <- sort(c(v_covar, v_src))

# Order columns
dat <- dat[,c(idVarsAux, v_cod, v_covarsrc)]

# Tidy
dat <- dat[order(dat$sid),]


# Save outputs ------------------------------------------------------------

write.csv(dat, paste0("./gen/update-covar-for-old-studies/output/Studies2020-UpdatedCovar_",ageSexSuffix,".csv"), row.names = FALSE)

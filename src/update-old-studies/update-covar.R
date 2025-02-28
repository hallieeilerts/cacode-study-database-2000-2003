################################################################################
#' @description Update national-level covariate values in old study database
#' Check that there are the same number of data points as in the old model input (5-19 only)
#' @return Old study database with updated national-level covariate values
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old age-specific study database in long format that now has covariate names and scales as pred database
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies-long_upd-names-scales_", ageSexSuffix, ".csv"))
}
## Old age-specific study database in long format that now has covariate names and scales as pred database, and study-level pfpr
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){ 
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies-long_merge-pfpr_", ageSexSuffix, ".csv"))
}
## Prediction database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
pred <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
## Old model inputs
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){
  if(ageSexSuffix == "05to09y"){load("./data/model-inputs-old/20201217-Data5to9-VAMCM009-Test3.RData")}
  if(ageSexSuffix == "10to14y"){load("./data/model-inputs-old/20201222-Data10to14-VAMCM009-Test8j.RData")}
  if(ageSexSuffix == "15to19yF"){load("./data/model-inputs-old/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
  if(ageSexSuffix == "15to19yM"){load("./data/model-inputs-old/20210212-Data15to19Men-VAMCM009-Test9e.RData")}
}
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key_covar <- read.csv(paste0("./gen/update-old-studies/output/", dat_filename, sep = ""))
## Key for which study covariates should be replaced
key_replace <- read.csv(paste0("./gen/update-old-studies/temp/key_covar-replacement_", ageSexSuffix, ".csv", sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# List of covariates in pred database
v_pred_covar <- unique(subset(key_covar, !is.na(pred))$pred)

# Reclassified CODs for this age group (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key_cod, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]

# Create temp id
dat$tempid <- 1:nrow(dat)

#dat %>% filter(id %in% "IND-1999-0401-138-0-12-T") %>% View

# Merge with replacement key and separate variables that need to be replaced from those that dont
df_replace <- merge(dat, key_replace, by = c("id","strata_id","variable"))
df_keep <- subset(dat, !(tempid %in% df_replace$tempid))
# If did not match to replacement key and source column is NA, this means we were missing the source column.
# It has been assumed that the data came from a subnational DHS value if it was possible to fetch it from there.
# All sources unknown in old neonatal studies (don't have source columns currently)
df_keep$source[is.na(df_keep$source)] <- "Assumed to be DHS subnational value"

# Merge on new prediction database values
df_replace <- merge(df_replace, pred[,c("iso3","year","variable","value_main")], by.x = c("iso3","year_mid","variable"), by.y = c("iso3","year","variable"))
df_replace$value <- df_replace$value_main
df_replace$value_main <- NULL
# Make new source column
df_replace$source <- paste("PredDB2023", df_replace$iso3, df_replace$year_mid, sep = "_")

# Recombine
studyupd <- rbind(df_keep, df_replace)
studyupd$tempid <- NULL

# Check if any covariate values are missing
if(nrow(subset(studyupd, is.na(value))) > 0){
  warning("covariate values are missing")
}
if(nrow(subset(studyupd, is.na(source))) > 0){
  warning("covariate sources are missing")
}

# Reshape data points wide
studyupdWide <- studyupd %>%
  pivot_wider(names_from = variable, values_from = c(value, source),
              names_glue = "{variable}_{.value}") %>%
  rename_with(function(c) str_replace(c, "_value", ""))

# Check
length(unique(studyupdWide$id)) == length(unique(dat$id))

# Check for 5-19
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){
  # Same number of study data points as model input?
  if(nrow(studyupdWide) != nrow(studies)){
    warning("Study database and model input have different number of data points.")
  }

  # Any differences in study id with model input?
  if(any(!(sort(studyupdWide$id) == sort(studies$id)))){
    warning("Study database and model input have differences in study IDs.")
  }
}

# Covariate columns
v_covar <- unique(v_pred_covar[v_pred_covar %in% names(studyupdWide)])
# Source columns
v_src <- paste0(v_covar, "_source", sep = "")
# Combined vector of covariate and source columns
v_covarsrc <- sort(c(v_covar, v_src))

# Tidy
studyupdWide <- studyupdWide[,c(idVars, "totdeaths", v_cod_reclass, v_covarsrc)]
studyupdWide <- studyupdWide[order(studyupdWide$recnr),]

# Save outputs ------------------------------------------------------------

write.csv(studyupdWide, paste0("./gen/update-old-studies/output/Studies2019-UpdatedCovar_",ageSexSuffix,".csv"), row.names = FALSE)


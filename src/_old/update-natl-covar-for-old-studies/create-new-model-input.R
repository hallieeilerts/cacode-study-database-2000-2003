################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs

# Set age group
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# Old study database with updated values for all where called for
updstudies <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/temp/studies-updated_", ageSexSuffix, ".csv",sep=""))

# Studies object from old model input
oldstudies <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/temp/studies_oldstudies_", ageSexSuffix,".csv", sep = ""))

# Key matching old study and new pred covariate names
key_covarnames <- read.csv("./gen/update-natl-covar-for-old-studies/temp/key_study-pred-covarnames.csv")

# 2000-2023 prediction database
pred <- read.csv("./data/prediction-database/CovariateDatabase2023-wide_20241003.csv")
# 
# # Key matching old study and new pred covariate names
# key_covarnames <- read.csv("./gen/update-natl-covar-for-old-studies/output/key_study-pred-covarnames.csv")
# key_replacement <- read.csv("./gen/update-natl-covar-for-old-studies/output/key_study-covar-replacement.csv")
# key_studyid <- read.csv("./gen/update-natl-covar-for-old-studies/output/key_studyid_",ageSexSuffix,".csv",sep ="")
################################################################################

# Old model input covariate names that are not in new updated study database
names(oldstudies)[!(names(oldstudies) %in% names(updstudies))]

# Rename old study covariate names to match new covariate names
# Could do this withe the key_covarnames, but it doesn't work perfectly because of how things we done last time.
# Last time there was an additional step where the sex-specific covariate name would be used if available.
# Can code this up better later.
if(ageSexSuffix == "05to09y"){
  names(oldstudies)[which(names(oldstudies) == "dtp3_mf")] <- "vac_dtp3"
  names(oldstudies)[which(names(oldstudies) == "edu_mean")] <- "edu_mean_mf"
  names(oldstudies)[which(names(oldstudies) == "height")] <- "height_mf"
  names(oldstudies)[which(names(oldstudies) == "lowest_wealth")] <- "wealth_lowest"
  names(oldstudies)[which(names(oldstudies) == "q5to19")] <- "mr05to19"
  names(oldstudies)[which(names(oldstudies) == "thinness")] <- "thinness_mf" 
  names(oldstudies)[which(names(oldstudies) == "unemployment_neet")] <- "unemployment_neet_mf"
}

# Keep old model input id var and merge with new values (updated with pred database where called for)
v_id <- c("sid",'id', 'reterm', 'totdeaths')
v_covar <- names(oldstudies)[!(names(oldstudies) %in% v_id)]
match1 <- merge(oldstudies[,v_id], updstudies[,c("id", v_covar)], by = "id")
nrow(oldstudies) == nrow(match1)
# Some not matched
# Some study IDs are different in the final model input
oldstudies$id[!(oldstudies$id %in% match1$id)]

# Remaining studies that need a match
remaining <- subset(oldstudies, !(id %in% match1$id))

# Make id variable without sex
updstudies$id2 <- sub("-M$|-F$|-B$", "", updstudies$id)
remaining$id2 <- sub("-M$|-F$|-B$", "", remaining$id)
# Try to merge again
match2 <- merge(remaining[,c("id2", v_id)], updstudies[,c("id2", v_covar)], by = "id2")
match2$id2 <- NULL
# Make sure no duplicates after merge
nrow(match2) == length(unique(match2$id))
nrow(match2) == nrow(remaining)
# Some not matched
# Some study IDs are different in the final model input
remaining$id[!(remaining$id %in% match2$id)]

# Remaining studies that need a match
remaining <- subset(oldstudies, !(id %in% c(match1$id, match2$id)))
unique(remaining$id)
# These look like data points from LMM that were added to HMM.
# Update these covariates with prediction database.
remaining$iso3 <- substr(remaining$id, 1, 3)
remaining$year <- substr(remaining$id, 5, 8)
match3 <- merge(remaining[,c(v_id,"iso3", "year")], pred[,c("iso3","year", v_covar)], by = c("iso3", "year"))
match3$iso3 <- match3$year <- NULL
# Make sure no duplicates after merge
nrow(match3) == length(unique(match3$id))
# All matched now
remaining$id[!(remaining$id %in% match3$id)]

# Combine
modinput <- rbind(match1, match2, match3)
nrow(oldstudies)
nrow(modinput)
unique(length(modinput$id))

write.csv(modinput, paste0("./gen/update-natl-covar-for-old-studies/temp/modinput_updatedcovar_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)



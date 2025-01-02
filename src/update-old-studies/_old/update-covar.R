################################################################################
#' @description Update covariate names and scales in old model inputs
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs
source("./src/update-covar-for-old-studies/set-inputs.R")
## Prediction database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
pred <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
## Old age-specific study database that now has covariate names and scales as pred database
studydb <- read.csv(paste0("./gen/update-covar-for-old-studies/temp/studydb_upd-names-scales_", ageSexSuffix, ".csv"))
## Key for which study covariates should be replaced
key <- read.csv(paste0("./gen/update-covar-for-old-studies/temp/key_covar-replacement_", ageSexSuffix, ".csv", sep = ""))
################################################################################

dat <- studies %>% 
  mutate(tempid = 1:n()) %>%
  mutate(iso3 = substr(id, 1, 3),
         year = substr(id, 5, 8))

# Merge with replacement key and separate variables that need to be replaced from those that dont
df_replace <- merge(dat, key, by = c("id","variable"))
df_keep <- subset(dat, !(tempid %in% df_replace$tempid))

# Covariates to add
# Since we are using the model objects, these do not contain all the available covariates (just the ones that were modeled)
# Not possible to use the HMMstudyDatabase because would have to mimic all the processing pancho did and can't get IDs to match.
# Want to have this code written differently.
# For now, just add on national level values for extra covariates
pred_add <- subset(pred[,c("iso3","year","variable","value_main")], !(variable %in% dat$variable))
dat_add <- dat
dat_add$variable <- dat_add$value <- NULL
dat_add <- dat_add[!duplicated(dat_add)]
df_add <- merge(dat_add, pred_add, by = c("iso3","year"))


# Merge on prediction database values
df_replace <- merge(df_replace, pred[,c("iso3","year","variable","value_main")], by = c("iso3","year","variable"))
df_replace$value <- df_replace$value_main
df_replace$value_main <- NULL



# Recombine
studyupd <- rbind(df_keep, df_replace)
studyupd$tempid <- NULL

# Reshape data points wide
studyupdWide <- studyupd %>%
  pivot_wider(names_from = variable, values_from = value) 

# Check
length(unique(studyupdWide$id))
length(unique(studies$id))

# Tidy
studyupdWide <- studyupdWide[order(studyupdWide$id),]

# Save outputs ------------------------------------------------------------

write.csv(studyupdWide, paste0("./gen/update-covar-for-old-studies/output/StudyCovar2020-Updated_",ageSexSuffix,".csv"), row.names = FALSE)

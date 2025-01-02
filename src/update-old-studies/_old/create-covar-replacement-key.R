################################################################################
#' @description Create covariate replacementkey
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
## Old model input that now has covariate names and scales as pred database
studies <- read.csv(paste0("./gen/update-covar-for-old-studies/temp/mod-input_upd-covar-names_", ageSexSuffix, ".csv"))
## Covariate data extraction from DHS
dat_filename <- list.files("./data/dhs")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
dhs <- read.csv(paste0("./data/dhs/", dat_filename, sep = ""))
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-covar-for-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key <- read.csv(paste0("./gen/update-covar-for-old-studies/output/", dat_filename, sep = ""))
################################################################################

# Create iso3 and year for studies
df_replace <- studies %>% 
  mutate(iso3 = substr(id, 1, 3),
         year = substr(id, 5, 8))

# Identify covariates that came from DHS survey and are subnational
v_dhs_covar <- unique(dhs$variable)
# Manually add covariates that have different names in dhs extraction
# Need to harmonize these names. Make dhs extraction match prediction database. Prediction database should take precedent.
v_dhs_covar <- c(v_dhs_covar, "vac_pab", "stunting", "underweight")
# Manually add covariates that aren't in dhs extraction this time (because not included in any model),
# but likely were pulled from dhs subnational in the past
v_dhs_covar <- c(v_dhs_covar, "imr_mf")

# Assign source to study covariates 
# Article and survey data points do not need to be updated
df_src <- df_replace %>%
  mutate(source_type = ifelse( variable %in% v_dhs_covar, "Survey", NA)) %>%
  mutate(source_type = ifelse( is.na(source_type) & variable %in% key$pred, "Prediction Database", source_type )) %>%
  mutate(source_type = ifelse( iso3 %in% "IND" &  !(variable %in% key$pred) & variable %in% key$predInd,
                               "India Subnational Prediction Database", source_type )) %>%
  mutate(replace = ifelse(source_type %in% c("Prediction Database"), TRUE, FALSE))

# Remove sex suffix
df_src$indicator <- sub("_f$|_m$|_mf$", "", df_src$variable)

# If any variable in the sex suffix group is from the survey, they should all be from survey
df_src <- df_src %>% 
  mutate(anysurv = ifelse(source_type == "Survey", 1, 0)) %>%
  group_by(indicator) %>%
  mutate(anysurv = max(anysurv))
df_src$source_type[df_src$anysurv == 1] <- "Survey"
df_src$replace[df_src$anysurv == 1] <- FALSE
df_src$anysurv <- df_src$indicator <-NULL

key <- df_src %>%
  filter(replace == TRUE) %>%
  select(id, variable)

# Save output -------------------------------------------------------------

write.csv(key, paste0("./gen/update-covar-for-old-studies/temp/key_study-covar-replacement_",ageSexSuffix,".csv",sep =""), row.names = FALSE)


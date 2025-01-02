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
## Old model inputs
if(ageSexSuffix == "05to09y"){load("./data/model-inputs-old/20201217-Data5to9-VAMCM009-Test3.RData")}
if(ageSexSuffix == "10to14y"){load("./data/model-inputs-old/20201222-Data10to14-VAMCM009-Test8j.RData")}
if(ageSexSuffix == "15to19yF"){load("./data/model-inputs-old/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
if(ageSexSuffix == "15to19yM"){load("./data/model-inputs-old/20210212-Data15to19Men-VAMCM009-Test9e.RData")}
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-covar-for-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key <- read.csv(paste0("./gen/update-covar-for-old-studies/output/", dat_filename, sep = ""))
################################################################################

# Id variables in old model inputs
v_id <- c("sid","id","reterm","totdeaths")

# Create sex suffix to help with merging on pred covariate names
# In the previous round, there was a step to use sex-specific covariates if available, but the covariate names in the model input don't have a sex suffix
if(ageSexSuffix %in% c("05to09y", "10to14y")){s_suffix <- "_mf"}
if(ageSexSuffix %in% c("15to19yF")){s_suffix <- "_f"}
if(ageSexSuffix %in% c("15to19yM")){s_suffix <- "_m"}

# Reshape studies in old model inputs to long
# Merge on covariate key (try with sex-suffix if no match)
# Adjust study scales
studylong <- studies %>% 
  pivot_longer(cols = !all_of(c(v_id)),
               names_to = c("study"), 
               values_to = "value"
  ) %>%
  left_join(key[,c("study","pred","study_adjustment")], by = join_by(study)) %>%
  mutate(study_sexspecific = paste0(study, s_suffix, sep ="")) %>%
  left_join(key[,c("study","pred","study_adjustment")], by = join_by(study_sexspecific == study)) %>% 
  mutate(variable = ifelse(!is.na(pred.x), pred.x, pred.y)) %>%
  mutate(study_adjustment = ifelse(!is.na(study_adjustment.x), study_adjustment.x, study_adjustment.y)) %>%
  mutate(value = ifelse(study_adjustment == "divide by 100", value/100, value)) %>%
  mutate(value = ifelse(study_adjustment == "subtract from 100 and divide by 100", (100-value)/100, value)) %>%
  select(-c(study, study_sexspecific, study_adjustment,
            pred.x, pred.y, study_adjustment.x, study_adjustment.y))


# # Reshape wide again
# studyupd <- studies %>% 
#   pivot_wider(id_cols = all_of(v_id),
#               names_from = variable,
#               values_from = value)


 
# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(studylong, paste0("./gen/update-covar-for-old-studies/temp/mod-input_upd-covar-names_", ageSexSuffix, ".csv"), row.names = FALSE)




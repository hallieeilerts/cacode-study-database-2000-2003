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
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# 2020 study database with new id variable
study <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/temp/studies-with-id_", ageSexSuffix, ".csv",sep=""))

# Key matching old study and new pred covariate names
key_covarnames <- read.csv("./gen/update-natl-covar-for-old-studies/temp/key_study-pred-covarnames.csv")

################################################################################

# Save study with same names and scales as pred db
# Steps:
# Reshape to long
# Merge on pred database variable names
# Adjust scales
# Drop variables that aren't in pred database
# Reshape to wide

# ID columns in study database
v_id <- c("id", "iso3", "year")

# List of covariates in study database
v_study_covar <- unique(subset(key_covarnames, !is.na(study))$study)

# Harmonize names of main study columns
studymain <- study %>% select(all_of(c(v_id, v_study_covar))) %>%
  pivot_longer(cols = !all_of(v_id),names_to = c("variable"), values_to = "value") %>% # Reshape to long
  mutate(study = variable) %>% # make new column with name of variable in prediction database
  left_join(.,key_covarnames, by = "study") %>% # join with key that links to study database
  select(all_of(c(v_id, "pred", "value"))) %>% 
  filter(!is.na(pred)) %>% 
  pivot_wider(names_from = pred, values_from = value)

# Harmonize scales

# Old study database uses zero_hundred whereas new prediction database uses zero_one
# Subset variables which are zero_one in new prediction database
v_zeroone <- unique(subset(key_covarnames, pred_scale == "zero_one"))$pred
names(studymain)[names(studymain) %in% v_zeroone]
# After checking scales plot. Make some manual adjustments.
# Do not divide by 100 for study covar that are already zero_one.
v_zeroone <- v_zeroone[!(v_zeroone %in% c("pop_male_15_29", "sab"))]
studymain[,names(studymain) %in% v_zeroone] <- studymain[,names(studymain) %in% v_zeroone]/100

# Recalculate what was contraception_unmet in old study db to be contraception_met
if("contraception_met" %in% names(studymain)){
  studymain[,names(studymain) %in% "contraception_met"] <- 1 - studymain[,names(studymain) %in% "contraception_met"]
}

studysource <- study %>% select(all_of(c(v_id, paste(v_study_covar, "_source", sep = ""))))  %>%
  pivot_longer(cols = !all_of(v_id),names_to = c("variable"), values_to = "value") %>% # Reshape to long
  mutate(study = str_replace(variable, "_source", "")) %>% # make new column with name of variable in prediction database
  left_join(.,key_covarnames, by = "study") %>% # join with key that links to study database
  select(all_of(c(v_id, "pred", "value"))) %>%
  filter(!is.na(pred)) %>% 
  mutate(pred = paste(pred, "_source", sep = "")) %>% # Add suffix onto pred database name
  pivot_wider(names_from = pred, values_from = value)

study_new <- studymain %>%
  full_join(., studysource, by = v_id)

# Order columns
v_col_order <- sort(names(study_new))
v_col_order <- v_col_order[!(v_col_order %in% v_id)]
study_new <- study_new[,c(v_id, v_col_order)]

nrow(study) == nrow(study_new)
nrow(study_new) == length(unique(study_new$id))

# Save output -------------------------------------------------------------

# Study database that now has same covariate names and scales as pred database
# Some covariates removed if they weren't in pred database
write.csv(study_new, paste0("./gen/update-natl-covar-for-old-studies/temp/studies-adjusted_", ageSexSuffix, ".csv"), row.names = FALSE)



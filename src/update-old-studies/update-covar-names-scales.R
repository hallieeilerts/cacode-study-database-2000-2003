################################################################################
#' @description Update names of old covariates and adjust scales
#' @return Study data points with covariates in long format, covariate names and scales updated to match new studies
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old study databases with updated id variables to match new studies
dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv", sep = ""))
## Harmonized study CODs in long format
datCOD <- read.csv(paste0("./gen/update-old-studies/temp/cod-wide_harmonized_", ageSexSuffix, ".csv", sep = ""))
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key_covar <- read.csv(paste0("./gen/update-old-studies/output/", dat_filename, sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# List of covariates in study database
v_study_covar <- unique(subset(key_covar, !is.na(study))$study)
# List of covariates in pred database
v_pred_covar <- unique(subset(key_covar, !is.na(pred))$pred)

## Harmonize covariate columns

# Reshape study covar to long
# Merge on covariate key
# Adjust scales where needed
# Replace old study covariate names with new prediction database names
# Drop covariates that are not in new prediction database
# If two study covariates matched to same prediction covariate, only keep one 
# Reshape covariates back wide

datCovar <- dat %>%
  select(all_of(idVars), any_of(v_study_covar)) %>%
  pivot_longer(cols = any_of(c(v_study_covar)),
               names_to = c("study"), 
               values_to = "value"
  ) %>%
  left_join(key_covar[,c("study","pred","study_adjustment")], by = join_by(study)) %>% 
  mutate(value = ifelse(study_adjustment == "divide by 100", value/100, value)) %>%
  mutate(value = ifelse(study_adjustment == "subtract from 100 and divide by 100", (100-value)/100, value)) %>%
  mutate(variable = ifelse(!is.na(pred), pred, study)) %>% 
  filter(!is.na(pred)) %>% # Drop if covariate is not in prediction database
  group_by(id, variable) %>%
  mutate(n = n_distinct(study)) %>% # Indicator for when multiple study covariates match same pred covariate (ors and ors_mf)
  filter(!(n > 1 & !(study %in% key_covar$pred))) %>% # Only keep the ones that appear in pred (ors_mf)
  select(-c(study, pred, study_adjustment, n)) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

# Check that all prediction database covariates are included
# If any are missing, add them with NA values.
v_missing <- v_pred_covar[!(v_pred_covar %in% names(dat))]
if(length(v_missing) > 1){
  for(i in 1:length(v_missing)){
    datCovar$missingCovar <- NA
    names(datCovar)[which(names(datCovar) == "missingCovar")] <- v_missing[i]
  }
}

# Reshape covariates long again
datCovarLong <- datCovar %>%
  select(all_of(idVars), any_of(v_pred_covar)) %>%
  pivot_longer(
    cols = any_of(c(v_pred_covar)),
    names_to = "variable", 
    values_to = "value"
  )

## Harmonize source columns

# If there are any _source columns, 
# Reshape source columns long
# Merge on covariate key
# Replace old source column names with new prediction database names
# Drop covariates that are not in new prediction database
# Merge with long covariates

if(any(grepl("_source", names(dat)))){
  datSrcLong <- dat %>% 
    select(all_of(idVars), ends_with("_source")) %>%
    rename_with(function(c) str_replace(c, "_source", "")) %>% 
    pivot_longer(cols = !any_of(idVars),
                 names_to = c("study"), 
                 values_to = "source"
    ) %>%
    left_join(key_covar[,c("study","pred")], by = join_by(study)) %>%
    mutate(variable = ifelse(!is.na(pred), pred, study)) %>%
    filter(!is.na(pred)) %>% # Drop if covariate is not in prediction database
    group_by(id, pred) %>%
    mutate(n = n_distinct(study)) %>% # Indicator for when multiple study covariates match same pred covariate (ors and ors_mf)
    filter(!(n > 1 & !(study %in% key_covar$pred))) %>% # Only keep the ones that appear in pred (ors_mf)
    ungroup() %>%
    select(-c(study, pred, n)) 

  # Merge on source column
  v_merge_col <- c(idVars, "variable")
  datCovarLong <- datCovarLong %>% left_join(., datSrcLong, by = v_merge_col)
}else{
  datCovarLong <- datCovarLong %>%
    mutate(source = NA)
}

# Merge back with CODs
datLong <- merge(datCOD, datCovarLong, by = idVars)
datLong <- datLong[order(datLong$recnr, datLong$variable),]

# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(datLong , paste0("./gen/update-old-studies/temp/studies-long_upd-names-scales_", ageSexSuffix, ".csv"), row.names = FALSE)


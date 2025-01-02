################################################################################
#' @description Update names of old COD, covariates, adjust scales of old covariates
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old study databases with updated id variables to match new studies
dat <- read.csv(paste0("./gen/update-covar-for-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv", sep = ""))
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-covar-for-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key_covar <- read.csv(paste0("./gen/update-covar-for-old-studies/output/", dat_filename, sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

## Useful vectors

# List of covariates in study database
v_study_covar <- unique(subset(key_covar, !is.na(study))$study)
# List of covariates in pred database
v_pred_covar <- unique(subset(key_covar, !is.na(pred))$pred)

# Reclassified CODs for this age group (includes Other and Undetermined)
df_reclass <- subset(key_cod, !is.na(cod_reclass))
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y)
df_reclass <- subset(df_reclass, cod_reclass != "TB")
# Exclude "Undetermined" which is used to eliminate studies in cleaning phase
df_reclass <- subset(df_reclass, cod_reclass != "Undetermined")
df_reclass <- df_reclass[,c("cod_mapped", "cod_reclass")]
v_cod_reclass <- unique(df_reclass$cod_reclass)
v_cod_mapped <- key_cod$cod_mapped


## Harmonize COD names

# Rename study COD names to match cod_mapped in COD reclassification key
# Reshape to COD long
# Merge on COD reclassification key
# Replaced mapped COD names with reclass COD names
# Reshape CODs back wide
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  datCOD <- dat %>% 
    rename_with(
      ~ case_when(
        . == "congenital" ~ "congen",
        . == "diarrhoea" ~ "dia",
        . == "diarrhea" ~ "dia",
        . == "pneumonia" ~ "lri",
        . == "neonatal" ~ "neonatal_cond",
        . == "meningitis" ~ "mening",
        . == "measles" ~ "mea",
        . == "malaria" ~ "mal",
        . == "unknown" ~ "undt",
        TRUE ~ .)
    ) %>%
    pivot_longer(cols = any_of(c(v_cod_mapped)),
                 names_to = c("cod_mapped"), 
                 values_to = "cod_n"
    ) %>% 
    left_join(key_cod[,c("cod_mapped","cod_reclass")], by = join_by(cod_mapped)) %>% 
    filter(!is.na(cod_reclass)) %>% # If the cause is not getting reclassified, deaths should be dropped. 
    select(-c(cod_mapped)) %>% 
    group_by(strata_id) %>%
    mutate(totdeaths2 = sum(cod_n, na.rm = TRUE)) %>% # !!!!!!!!! recalculate totdeaths
    pivot_wider(
      names_from = cod_reclass,
      values_from = cod_n
    ) %>% 
    select(all_of(idVars), "totdeaths", "totdeaths2", any_of(v_cod_reclass)) # Using any_of because old neonate data doesn't have Other
}
if(ageSexSuffix %in% c("01to59m")){
  datCOD <- dat %>% 
    rename_with(
      ~ case_when(
        . == "congenital" ~ "congen",
        . == "diarrhoea" ~ "dia",
        . == "diarrhea" ~ "dia",
        . == "pneumonia" ~ "lri",
        . == "neonatal" ~ "neonatal_cond",
        . == "meningitis" ~ "mening",
        . == "measles" ~ "mea",
        . == "malaria" ~ "mal",
        . == "unknown" ~ "undt",
        TRUE ~ .)
    ) %>%
    pivot_longer(cols = any_of(c(v_cod_mapped)),
                 names_to = c("cod_mapped"), 
                 values_to = "cod_n"
    ) %>% 
    left_join(key_cod[,c("cod_mapped","cod_reclass")], by = join_by(cod_mapped)) %>% 
    filter(!is.na(cod_reclass)) %>% # If the cause is not getting reclassified, deaths should be dropped. 
    select(-c(cod_mapped)) %>% 
    group_by(strata_id) %>%
    mutate(totdeaths2 = sum(cod_n, na.rm = TRUE)) %>% # !!!!!!!!! recalculate totdeaths
    pivot_wider(
      names_from = cod_reclass,
      values_from = cod_n
    ) %>% 
    select(all_of(idVars), "totdeaths", "totdeaths2", any_of(v_cod_reclass)) # Using any_of because old neonate data doesn't have Other
  
  
  # Investigate how causes added up to totdeaths
  # dat %>%
  #   select(article_id, comment, iso3, malaria, other, other_orig, other_nomaln, 
  #          injuries, measles, meningitis, neonatal, pneumonia, congenital, malnutrition, hiv, 
  #          diarrhea, unknown, stillbirth, totdeaths_orig, totdeaths) %>% 
  #   rowwise() %>%
  #   mutate(test1 = ifelse(is.na(other_nomaln), # there is no other_nomaln, also dont include malnutrition
  #                         sum(malaria, other, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea,  na.rm = T),
  #                         sum(malaria, other_nomaln, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, na.rm = T)
  #                         )) %>%
  #   filter(abs(totdeaths-test1) > 1) %>% View
  # Some test1 don't add up to totdeaths, but in the "comment" it says this is because "only some causes listed"
  
  
  # 1. If malnutrition is reported and other_nomaln is not, reduce other by the amount in malnutrition
  # 2. If malnurition is reported and other_nomaln is, recode other_nomaln as other
  # 3. If totdeaths is not found by adding up...
  #     malaria, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, and the new 'other'
  #     Add residual deaths to other.
  #     This only happens when there is a "comment" that says "only some causes listed"
  #     Note that totdeaths does not include hiv, measles, unknown.
  #     It needs to be recalculated after cause mapping applied
  dat <- dat %>%
    mutate(other = ifelse(!is.na(malnutrition) & is.na(other_nomaln), other-malnutrition, other)) %>%
    mutate(other = ifelse(!is.na(malnutrition) & !is.na(other_nomaln), other_nomaln, other)) %>%
    rowwise() %>%
    mutate(checksum = sum(malaria, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, other,  na.rm = T)) %>%
    mutate(other = ifelse(abs(totdeaths-checksum) > 1, totdeaths - checksum, other))
  
  
  
}


# Update COD names to match this round
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF", "15to19yM")){
  datCOD <- dat %>% 
    rename_with(
      ~ case_when(
        . == "OtherCD" ~ "OtherCMPN" ,
        . == "RTA" ~ "RTI",
        . == "Other_inj" ~ "OtherInj",
        . == "Interp_violence" ~ "InterpVio" ,
        . == "Self_harm" ~ "SelfHarm",
        TRUE ~ .)
    ) %>%
    select(all_of(idVars), "totdeaths", all_of(v_cod_reclass))
}

# Check that all reclassified COD are reported
v_missing <- v_cod_reclass[!(v_cod_reclass %in% names(datCOD))]
if(length(v_missing) > 0){
  warning("Missing some expected CODs in old study database.")
  for(i in 1:length(v_missing)){
    datCOD$missingCOD <- NA
    names(datCOD)[which(names(datCOD) == "missingCOD")] <- v_missing[i]
  }
}

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
  mutate(n = uniqueN(study)) %>% # Indicator for when multiple study covariates match same pred covariate (ors and ors_mf)
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
    mutate(n = uniqueN(study)) %>% # Indicator for when multiple study covariates match same pred covariate (ors and ors_mf)
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

# Save output -----------------pred# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(datLong , paste0("./gen/update-covar-for-old-studies/temp/studies-long_upd-names-scales_", ageSexSuffix, ".csv"), row.names = FALSE)


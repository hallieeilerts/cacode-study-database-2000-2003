################################################################################
#' @description For neonates and postneonates, mapp and aggregate CODs
#' For 5-19, update names of old CODs
#' @return Study CODs aggregated by reclassified categories in long format 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old study databases with under-5 duplicates dropped
if(ageSexSuffix %in% c("00to28d","01to59m")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_dup-dropped_", ageSexSuffix, ".csv", sep = ""))
}
## Old study databases with updated id variables to match new studies
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF", "15to19yM")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv", sep = ""))
}
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
df_reclass <- subset(key_cod, !is.na(cod_reclass))
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y)
df_reclass <- subset(df_reclass, cod_reclass != "TB")
# Exclude "Undetermined" which is used to eliminate studies in cleaning phase
df_reclass <- subset(df_reclass, cod_reclass != "Undetermined")
df_reclass <- df_reclass[,c("cod_mapped", "cod_reclass")]
v_cod_reclass <- unique(df_reclass$cod_reclass)
v_cod_mapped <- key_cod$cod_mapped

# Rename study COD names to match cod_mapped in COD reclassification key
# Reshape to COD long
# Merge on COD reclassification key
# Drop any COD not being reclassified
# Replaced mapped COD names with reclass COD names
# Reshape CODs back wide and aggregate reclassified COD
if(ageSexSuffix %in% c("00to28d")){
  
  datCOD <- dat %>% 
    rename_with(
      ~ case_when(
        . == "congenital" ~ "congen",
        . == "diarrhoea" ~ "dia",
        . == "pneumonia" ~ "lri",
        TRUE ~ .)
    ) %>%
    pivot_longer(cols = any_of(c(v_cod_mapped)),
                 names_to = c("cod_mapped"), 
                 values_to = "cod_n"
    ) %>% 
    left_join(df_reclass[,c("cod_mapped","cod_reclass")], by = join_by(cod_mapped)) %>% 
    filter(!is.na(cod_reclass)) %>% # If the cause is not getting reclassified, deaths should be dropped. 
    select(-c(cod_mapped)) %>% 
    mutate(cod_n = round(cod_n)) %>% # Round COD n to whole numbers. Actually already whole for neonates. Old needs to be done for 1-59m but including for consistency.
    group_by(strata_id) %>% 
    mutate(totdeaths_noOther = sum(cod_n, na.rm = TRUE)) %>% # Sum up deaths from all reported causes (old neonatal data doesn't include "Other")
    group_by(strata_id, cod_reclass) %>% 
    mutate(codReclassN = n()) %>% # If multiple mapped causes are going to be aggregated into a reclassified cause,
    mutate(cod_n = ifelse(is.na(cod_n) & codReclassN > 1, 0, cod_n)) %>% # Assign zero to any NA values for the reclassified cause. 
    # This will allow them to sum easily, while still retaining other NA values for causes not reported in the study at all.
    # Only seems necessary for 1-59m, but including here too just in case.
    pivot_wider(
      names_from = cod_reclass,
      values_from = cod_n
    ) %>% 
    mutate(Other = totdeaths - totdeaths_noOther) %>% # Recover Other from difference between totdeaths and sum of reported causes
    select(all_of(idVars), "totdeaths", any_of(v_cod_reclass)) # Using any_of because old neonate data doesn't have Other
  
  # Check that totdeaths is equal to sum of causes
  totDif <- which(datCOD$totdeaths != apply(datCOD[, paste0(v_cod_reclass)], 1, sum, na.rm = T))
  if(length(totDif)>0){
    warning("Sum of causes does not equal totdeaths.")
  }
}

if(ageSexSuffix %in% c("01to59m")){
  
  # There are a number of extra COD categories in the old 1-59m study data
  # If we want to extract malnutrition, need to figure out which "other" category to use.
  # To do so, investigate how causes added up to totdeaths.
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
  # Plan:
  # 1. If malnutrition is reported and other_nomaln is not, reduce "other" by the amount in malnutrition
  # 2. If malnurition is reported and other_nomaln is, recode "other_nomaln" as "other"
  # 3. If totdeaths is not found by adding up...
  #     malaria, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, and the new "other"
  #     Add residual deaths to other.
  #     This only happens when there is a "comment" that says "only some causes listed"
  #     Note that totdeaths does not include hiv, measles, unknown.
  #     It thus needs to be recalculated after cause mapping applied
  
  datRecalcOther <- dat %>%
    mutate(other = ifelse(!is.na(malnutrition) & is.na(other_nomaln), other-malnutrition, other)) %>%
    mutate(other = ifelse(!is.na(malnutrition) & !is.na(other_nomaln), other_nomaln, other)) %>%
    rowwise() %>%
    mutate(checksum = sum(malaria, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, other,  na.rm = T)) %>%
    mutate(other = ifelse(abs(totdeaths-checksum) > 1, totdeaths - checksum, other)) %>%
    select(-c(totdeaths_orig, other_nomaln, maln_int, other_int, other_orig, checksum))
  
  # Rename study COD names to match cod_mapped in COD reclassification key
  # Reshape to COD long
  # Merge on COD reclassification key, drop causes not getting reclassified
  # Drop any COD not being reclassified
  # Recalculate total deaths
  # Replaced mapped COD names with reclass COD names
  # Reshape CODs back wide and aggregate reclassified COD
  datCOD <- datRecalcOther %>% 
    rename_with(
      ~ case_when(
        . == "congenital" ~ "congen",
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
    left_join(df_reclass[,c("cod_mapped","cod_reclass")], by = join_by(cod_mapped)) %>% 
    filter(!is.na(cod_reclass)) %>% # If the cause is not getting reclassified, deaths should be dropped. 
    select(-c(cod_mapped)) %>% 
    mutate(cod_n = round(cod_n)) %>% # Round COD n to whole numbers
    group_by(strata_id) %>% 
    mutate(totdeaths = sum(cod_n, na.rm = TRUE)) %>% # recalculate totdeaths
    group_by(strata_id, cod_reclass) %>% 
    mutate(codReclassN = n()) %>% # If multiple mapped causes are going to be aggregated into a reclassified cause,
    mutate(cod_n = ifelse(is.na(cod_n) & codReclassN > 1, 0, cod_n)) %>% # Assign any NA values for the reclassified cause as zero. 
    # This will allow them to sum easily while still keeping any other NA values for causes not reported in the study.
    pivot_wider(
      names_from = cod_reclass,
      values_from = cod_n,
      values_fn=sum
    ) %>% 
    select(all_of(idVars), "totdeaths", any_of(v_cod_reclass)) # Using any_of because old neonate data doesn't have Other
  
  # Check that totdeaths is equal to sum of causes
  totDif <- which(datCOD$totdeaths != apply(datCOD[, paste0(v_cod_reclass)], 1, sum, na.rm = T))
  if(length(totDif)>0){
    warning("Sum of causes does not equal totdeaths.")
  }
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


# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(datCOD , paste0("./gen/update-old-studies/temp/cod-long_harmonized_", ageSexSuffix, ".csv"), row.names = FALSE)

